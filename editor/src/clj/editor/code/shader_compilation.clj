;; Copyright 2020-2025 The Defold Foundation
;; Copyright 2014-2020 King
;; Copyright 2009-2014 Ragnar Svensson, Christian Murray
;; Licensed under the Defold License version 1.0 (the "License"); you may not use
;; this file except in compliance with the License.
;;
;; You may obtain a copy of the License, together with FAQs at
;; https://www.defold.com/license
;;
;; Unless required by applicable law or agreed to in writing, software distributed
;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations under the License.

(ns editor.code.shader-compilation
  (:require [clojure.string :as string]
            [dynamo.graph :as g]
            [editor.build-target :as bt]
            [editor.protobuf :as protobuf]
            [editor.resource :as resource]
            [editor.workspace :as workspace]
            [util.coll :as coll]
            [util.eduction :as e])
  (:import [com.dynamo.bob.pipeline ShaderProgramBuilder$ShaderDescBuildResult ShaderProgramBuilderEditor Shaderc$ShaderResource]
           [com.dynamo.bob.pipeline.shader SPIRVReflector]
           [com.dynamo.bob.pipeline.shader ShaderCompilePipeline$ShaderModuleDesc]
           [com.dynamo.graphics.proto Graphics$ShaderDesc Graphics$ShaderDesc$Language Graphics$ShaderDesc$ShaderType]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def built-pb-class Graphics$ShaderDesc)

(defn- shader-language-to-java
  ^Graphics$ShaderDesc$Language [language]
  (case language
    :language-glsl-sm120 Graphics$ShaderDesc$Language/LANGUAGE_GLSL_SM120
    :language-glsl-sm430 Graphics$ShaderDesc$Language/LANGUAGE_GLSL_SM430
    :language-gles-sm100 Graphics$ShaderDesc$Language/LANGUAGE_GLES_SM100
    :language-gles-sm300 Graphics$ShaderDesc$Language/LANGUAGE_GLES_SM300
    :language-glsl-sm330 Graphics$ShaderDesc$Language/LANGUAGE_GLSL_SM330
    :language-spirv Graphics$ShaderDesc$Language/LANGUAGE_SPIRV))

(defonce ^:private ^"[Lcom.dynamo.graphics.proto.Graphics$ShaderDesc$Language;" java-shader-languages-with-spirv
  (into-array
    Graphics$ShaderDesc$Language
    (map shader-language-to-java
         ;; TODO: WGSL support (:language-wgsl)
         [:language-glsl-sm330 :language-gles-sm300 :language-gles-sm100 :language-glsl-sm430 :language-spirv])))

(defn shader-type-from-ext
  ^Graphics$ShaderDesc$ShaderType [^String resource-type-ext]
  (case resource-type-ext
    "fp" Graphics$ShaderDesc$ShaderType/SHADER_TYPE_FRAGMENT
    "vp" Graphics$ShaderDesc$ShaderType/SHADER_TYPE_VERTEX
    "cp" Graphics$ShaderDesc$ShaderType/SHADER_TYPE_COMPUTE))

(defn make-shader-module-desc
  ^ShaderCompilePipeline$ShaderModuleDesc [^String resource-type-ext ^String resource-proj-path ^String shader-source]
  {:pre [(string? resource-proj-path)
         (pos? (count resource-proj-path))
         (string? shader-source)
         (pos? (count shader-source))]}
  (let [shader-type (shader-type-from-ext resource-type-ext)
        shader-module-desc (ShaderCompilePipeline$ShaderModuleDesc.)]
    (set! (. shader-module-desc source) shader-source)
    (set! (. shader-module-desc resourcePath) resource-proj-path)
    (set! (. shader-module-desc type) shader-type)
    shader-module-desc))

(defn make-shader-desc-with-variants
  ^ShaderProgramBuilder$ShaderDescBuildResult [build-resource shader-module-descs ^long max-page-count]
  {:pre [(workspace/build-resource? build-resource)]}
  (let [build-resource-path (resource/path build-resource)
        shader-module-descs-array (into-array ShaderCompilePipeline$ShaderModuleDesc shader-module-descs)]
    (ShaderProgramBuilderEditor/makeShaderDescWithVariants build-resource-path shader-module-descs-array java-shader-languages-with-spirv max-page-count)))

(defn- error-string->error-value [^String error-string]
  (g/error-fatal (string/trim error-string)))

(defn- build-shader [build-resource _dep-resources user-data]
  (let [max-page-count (:max-page-count user-data)
        shader-module-descs (e/map (fn [{:keys [ext proj-path shader-source]}]
                                     (make-shader-module-desc ext proj-path shader-source))
                                   (:shader-infos user-data))
        result (make-shader-desc-with-variants build-resource shader-module-descs max-page-count)
        compile-warning-messages (.-buildWarnings result)
        compile-error-values (mapv error-string->error-value compile-warning-messages)]
    (g/precluding-errors compile-error-values
      {:resource build-resource
       :content (protobuf/pb->bytes (.-shaderDesc result))})))

(defn make-shader-build-target [node-id shader-source-infos max-page-count]
  {:pre [(g/node-id? node-id)
         (vector? shader-source-infos)
         (pos? (count shader-source-infos))
         (integer? max-page-count)]}
  (let [workspace (resource/workspace (:resource (first shader-source-infos)))

        shader-infos
        (mapv (fn [{:keys [resource shader-source]}]
                {:pre [(workspace/source-resource? resource)
                       (string? shader-source)
                       (pos? (count shader-source))]}
                {:ext (resource/type-ext resource)
                 :proj-path (resource/proj-path resource)
                 :shader-source shader-source})
              shader-source-infos)]

    (bt/with-content-hash
      {:node-id node-id
       :resource (workspace/make-placeholder-build-resource workspace "sp")
       :build-fn build-shader
       :user-data {:max-page-count max-page-count
                   :shader-infos shader-infos}})))

;; A resource namespace is the first literal up until the first dot in a
;; resource binding. For example, if we have a uniform buffer with some nested
;; data types:
;;
;;   struct MyMaterial {
;;     vec4 diffuse;
;;     vec4 specular;
;;   };
;;
;;   uniform my_uniforms {
;;     MyMaterial material;
;;   };
;;
;; When crosscompiled to SM120 (which is used by the editor), we will get two
;; uniforms:
;;   _<id>.material.diffuse
;;   _<id>.material.specular
;;
;; To be able to map this in a material constant, we need to strip the namespace
;; from the reflected data when the shader is created (see the implementation of
;; editor.gl.shader/make-shader-program) since there is no way a user can know
;; what the generated id will be for older shaders.
(defn- resource-binding-namespaces [^SPIRVReflector reflector]
  ;; Storage buffers (also known as SSBOs) will need the same mapping as uniform
  ;; buffers, but since we don't support them in the editor yet, we don't gather
  ;; their namespaces here.
  (mapv (fn [^Shaderc$ShaderResource uniform-buffer-object]
          (str "_" (.id uniform-buffer-object)))
        (.getUBOs reflector)))

(defn transpile-shader-source [shader-proj-path shader-ext ^String shader-source ^long max-page-count]
  (let [shader-type (shader-type-from-ext shader-ext)
        shader-language (shader-language-to-java :language-glsl-sm120) ; Use the old GLES2-compatible shaders.
        result (ShaderProgramBuilderEditor/buildGLSLVariantTextureArray shader-proj-path shader-source shader-type shader-language max-page-count)
        full-source (.source result)
        array-sampler-names-array (.arraySamplers result)
        reflector (.reflector result)]
    {:shader-source full-source
     :resource-binding-namespaces (resource-binding-namespaces reflector)
     :array-sampler-names (vec array-sampler-names-array)}))

(comment
  ;; TODO(instancing): Generate VertexDescription from compiled shader reflection. Something like this?
  (let [result (:result (get @stuff "/builtins/materials/model.fp"))
        ^SPIRVReflector reflector (.-reflector result)]
    (coll/transfer (.getInputs reflector) []
      (map (fn [^Shaderc$ShaderResource input]
             (sorted-map
               :name (.-name input)
               :name-hash (.-nameHash input)
               :instance-name (.-instanceName input)
               :instance-name-hash (.-instanceNameHash input)
               :type (let [type (.-type input)]
                       (sorted-map
                         :base-type (str (.-baseType type))
                         :dimension-type (str (.-dimensionType type))
                         :image-storage-type (str (.-imageStorageType type))
                         :image-access-qualifier (str (.-imageAccessQualifier type))
                         :image-base-type (str (.-imageBaseType type))
                         :type-index (.-typeIndex type)
                         :vector-size (.-vectorSize type)
                         :column-count (.-columnCount type)
                         :array-size (.-arraySize type)
                         :use-type-index (.-useTypeIndex type)
                         :image-is-arrayed (.-imageIsArrayed type)
                         :image-is-storage (.-imageIsStorage type)))
               :id (.-id input)
               :block-size (.-blockSize input)
               :location (.-location input)
               :binding (.-binding input)
               :set (.-set input)
               :stage-flags (.-stageFlags input)))))))
