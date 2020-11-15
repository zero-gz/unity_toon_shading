Shader "my_shader/toon_pbr_mix"
{
	Properties
	{
		[KeywordEnum(DEFAULT, SUBSURFACE, SKIN, HAIR)] _LIGHTING_TYPE("shading model", Float) = 0

		[Space(10)]
		[Toggle]ENABLE_DIFFUSE("ENABLE_DIFFUSE", Float) = 1
		[Toggle]ENABLE_PBR_SPECULAR("ENABLE_PBR_SPECULAR", Float) = 1
		[Toggle]ENABLE_GI("ENABLE_GI", Float) = 1
		_toon_diffuse_intensity("toon diffuse intensity", Range(0, 5)) = 1.0
		_pbr_specular_intensity("pbr specular intensity", Range(0, 5)) = 1.0
		_gi_intensity("gi intensity", Range(0, 5)) = 1.0

		[Space(30)]
		_albedo_tex ("albedo texture", 2D) = "white" {}
		_MainColor("Main Color", Color) = (1,1,1)
		_ShadowColor("Shadow Color", Color) = (0.7, 0.7, 0.8)
		_ShadowRange("Shadow Range", Range(0, 1)) = 0.5
		_ShadowSmooth("Shadow Smooth", Range(0, 1)) = 0.2
		_tex_ctrl_threshold("ilm texture g channel", Range(0, 1)) = 0.5

		[Space(30)]
		_normal_tex("normal texture", 2D) = "bump"{}
		_metallic("metallic", Range(0, 1)) = 0
		_roughness("roughness", Range(0, 1)) = 0
		_mix_tex("mix texture (R metallic, G roughness)", 2D) = "black" {}

		[Space(20)]
		[HDR]_emissive("Emissive", Color) = (0.0, 0.0, 0.0, 0.0)

		[Space(30)]
		[Toggle]ENABLE_RIMLIGHT("ENABLE_RIMLIGHT", Float) = 0
		_RimColor("Rim Color", Color) = (0,0,0,1)
		_RimMin("Rim min", Range(0,1)) = 0
		_RimMax("Rim max", Range(0, 1)) = 1
		_RimSmooth("Rim smooth", Range(0, 1)) = 1

		[Space(30)]
		[KeywordEnum(NORMAL, VERTEX)] MOVE("seconad pass move mode", Float) = 0
		[Toggle]USE_TANGENT("USE_TANGENT", Float) = 0
		_OutlineWidth("Outline Width", Range(0.01, 2)) = 0.24
		_OutLineColor("OutLine Color", Color) = (0.5,0.5,0.5,1)
		_move_distance("Move distance", Range(0, 0.3)) = 0.01

		[Space(30)]
		//hair相关
		_anisotropy("anisortopy", Range(-1.0, 1.0)) = 0.0
		_anisotropy_intensity("_anisotropy_intensity", Range(0.1, 10.0)) = 1.0
		_hair_jitter("hair jitter", 2D) = "black" {}
		_jitter_scale("jitter scale", Range(0, 10)) = 0.0
		//_hair_tangent("hair tangent", 2D) = "white" {}

		[Space(30)]
		//skin相关
		_preinteger_tex("preinteger tex", 2D) = "white" {}
		_sss_tex("sss tex", 2D) = "white" {}
	}
	SubShader
	{
		// 这里的tags要这么写，不然阴影会有问题
		Tags { "RenderType" = "Opaque" "Queue" = "Geometry"}
		LOD 100

		Pass
		{
		// 这个ForwardBase非常重要，不加这个， 光照取的结果都会跳变……
		Tags {"LightMode" = "ForwardBase"}
		CGPROGRAM
		#pragma target 5.0
		#pragma vertex vert
		#pragma fragment frag
		#pragma multi_compile LIGHTMAP_OFF LIGHTMAP_ON
		#pragma multi_compile DYNAMICLIGHTMAP_OFF DYNAMICLIGHTMAP_ON
		#pragma multi_compile __ DIRLIGHTMAP_COMBINED
		//#pragma multi_compile __ UNITY_SPECCUBE_BOX_PROJECTION   //奇怪了，不开启这个也能生效，环境球反射……
		#pragma multi_compile __ LIGHTPROBE_SH

		#pragma multi_compile_fwdbase
		#pragma enable_d3d11_debug_symbols
		#pragma shader_feature _LIGHTING_TYPE_DEFAULT _LIGHTING_TYPE_SUBSURFACE _LIGHTING_TYPE_SKIN _LIGHTING_TYPE_HAIR

			float _sss_strength;
			float _anisotropy;
			float _anisotropy_intensity;
			sampler _hair_jitter;
			float _jitter_scale;
			sampler _hair_tangent;

			half3 _MainColor;
			half3 _ShadowColor;
			half _ShadowRange;
			float _ShadowSmooth;
			float _tex_ctrl_threshold;

			float _toon_diffuse_intensity;
			float _pbr_specular_intensity;
			float _gi_intensity;

			float4 _RimColor;
			float _RimMin;
			float _RimMax;
			float _RimSmooth;
			
			#pragma shader_feature ENABLE_DIFFUSE_ON
			#pragma shader_feature ENABLE_PBR_SPECULAR_ON
			#pragma shader_feature ENABLE_GI_ON
			#pragma shader_feature ENABLE_RIMLIGHT_ON			
			
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "AutoLight.cginc"

			#include "common.cginc"
			#include "brdf.cginc"
			#include "gi_lighting.cginc"

			//#include "effects.cginc"

			v2f vert (appdata v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uv = v.uv;

				o.world_pos = mul(unity_ObjectToWorld, v.vertex).xyz;
				o.world_normal = mul(v.normal.xyz, (float3x3)unity_WorldToObject);
				o.world_tangent = normalize(mul((float3x3)unity_ObjectToWorld, v.tangent.xyz));
				o.world_binnormal = cross(o.world_normal, o.world_tangent)*v.tangent.w;

   				#ifdef LIGHTMAP_ON
					o.lightmap_uv.xy = v.uv1.xy * unity_LightmapST.xy + unity_LightmapST.zw;
					o.lightmap_uv.zw = 0;
				#endif

				#ifdef DYNAMICLIGHTMAP_ON
					o.lightmap_uv.zw = v.uv2.xy * unity_DynamicLightmapST.xy + unity_DynamicLightmapST.zw;
				#endif

				o.screen_pos = ComputeScreenPos(o.pos);
				TRANSFER_SHADOW(o);

				return o;
			}

			MaterialVars gen_material_vars(v2f i)
			{
				MaterialVars mtl;
				float4 albedo_color =  tex2D(_albedo_tex, i.uv);
				mtl.albedo = albedo_color.rgb;

				float3 normal_color = tex2D(_normal_tex, i.uv).rgb;
				mtl.normal = normal_color*2.0 - 1.0;
#if defined(_LIGHTING_TYPE_CLEARCOAT)
				float3 clearcoat_normal_color = tex2D(_clearcoat_normal_tex, i.uv).rgb;
				mtl.clearcoat_normal = clearcoat_normal_color * 2.0 - 1.0;
#endif

				mtl.roughness = _roughness;// tex2D(_mix_tex, i.uv).g; //_roughness;
				mtl.metallic = _metallic;// tex2D(_mix_tex, i.uv).r; //_metallic;
				mtl.emissive = _emissive;
				mtl.opacity = albedo_color.a;
				mtl.occlusion = 1.0;

				mtl.sss_color = _sss_color.rgb;

				float4 sss_tex_data = tex2D(_sss_tex, i.uv);
				mtl.thickness = sss_tex_data.r;
				mtl.curvature = sss_tex_data.g;
				return mtl;
			}

			LightingVars gen_lighting_vars(v2f i, MaterialVars mtl)
			{
				LightingVars data;
				data.T = normalize(i.world_tangent);
				data.B = normalize(i.world_binnormal);
				data.N = normalize(normalize(i.world_tangent) * mtl.normal.x + normalize(i.world_binnormal) * mtl.normal.y + normalize(i.world_normal) * mtl.normal.z);
#if defined(_LIGHTING_TYPE_CLEARCOAT)
				data.clearcoat_N = normalize(normalize(i.world_tangent) * mtl.clearcoat_normal.x + normalize(i.world_binnormal) * mtl.clearcoat_normal.y + normalize(i.world_normal) * mtl.clearcoat_normal.z);
#endif

				data.V = normalize(_WorldSpaceCameraPos.xyz - i.world_pos.xyz);
				data.L = normalize(_WorldSpaceLightPos0.xyz);
				data.H = normalize(data.V + data.L);
				data.albedo = mtl.albedo;
				data.diffuse_color = mtl.albedo*(1.0 - mtl.metallic);
				data.f0 = lerp(float3(0.04, 0.04, 0.04), mtl.albedo, mtl.metallic);
				data.roughness = mtl.roughness;
				data.metallic = mtl.metallic;
				data.sss_color = mtl.sss_color;
				data.thickness = mtl.thickness;
				data.curvature = mtl.curvature;
				data.opacity = mtl.opacity;

				data.light_color = _LightColor0.rgb;

				#if defined(LIGHTMAP_ON) || defined(DYNAMICLIGHTMAP_ON)
					data.lightmap_uv = i.lightmap_uv;
				#endif

				data.world_pos = i.world_pos;

				data.occlusion = mtl.occlusion;
				data.shadow = UNITY_SHADOW_ATTENUATION(i, data.world_pos);

				data.base_vars.pos = i.pos;
				data.base_vars.uv0 = i.uv;
				data.pos = i.pos;
				return data;
			}

			float3 toon_diffuse(LightingVars data)
			{
				half halfLambert = dot(data.N, data.L) * 0.5 + 0.5;
				half threshold = saturate((halfLambert + _tex_ctrl_threshold)*0.5 - _ShadowRange);
				half ramp = smoothstep(0, _ShadowSmooth, threshold);
				half3 diffuse = lerp(_ShadowColor, _MainColor, ramp);
				return diffuse * data.diffuse_color * data.light_color;
			}

			//fixed4 frag (v2f i, out float depth:SV_Depth) : SV_Target
			fixed4 frag(v2f i) : SV_Target
			{
				float depth;
				MaterialVars mtl = gen_material_vars(i);
				LightingVars data = gen_lighting_vars(i, mtl);

				data = gen_lighting_vars(i, mtl);

				fixed3 final_color = 0;
				// lighting part
				//LightingResult dir_result = direct_blinnphone_lighting(data);
				LightingResult dir_result = direct_lighting(data);
				float3 toon_color = toon_diffuse(data);

#ifdef ENABLE_DIFFUSE_ON
				final_color += toon_color* _toon_diffuse_intensity + mtl.emissive + dir_result.lighting_scatter;
#endif

#ifdef ENABLE_PBR_SPECULAR_ON
			#ifdef _LIGHTING_TYPE_SKIN
				final_color += dir_result.lighting_diffuse*_pbr_specular_intensity;
			#else
				final_color += dir_result.lighting_specular*_pbr_specular_intensity;
			#endif
#endif

#ifdef ENABLE_GI_ON
				//GI的处理
				LightingResult gi_result = gi_lighting(data);
				final_color += (gi_result.lighting_diffuse + gi_result.lighting_specular)*data.occlusion*_gi_intensity;
#endif

#ifdef ENABLE_RIMLIGHT_ON
				// 需要加上一个边缘光mask
				half f = 1.0 - saturate(dot(data.V, data.N));
				half rim = smoothstep(_RimMin, _RimMax, f);
				rim = smoothstep(0, _RimSmooth, rim);
				half3 rimColor = rim * _RimColor.rgb *  _RimColor.a;

				final_color += rimColor;
#endif
				// sample the texture
				return fixed4(final_color, mtl.opacity);
			}
			ENDCG
		}


		// Pass to render object as a shadow caster
		
		Pass {
			Tags { "LightMode" = "ShadowCaster" }
			
			CGPROGRAM
			
			#pragma vertex vert
			#pragma fragment frag
			
			#pragma multi_compile_shadowcaster
			
			#include "UnityCG.cginc"
			
			struct v2f {
				V2F_SHADOW_CASTER;
				float2 uv:TEXCOORD1;
			};
			
			v2f vert(appdata_base v) {
				v2f o;
				
				TRANSFER_SHADOW_CASTER_NORMALOFFSET(o)
				
				o.uv = v.texcoord;
				
				return o;
			}
			
			fixed4 frag(v2f i) : SV_Target {				
				SHADOW_CASTER_FRAGMENT(i)
			}
			ENDCG
		}

			Pass
			{
				Tags {"LightMode" = "ForwardBase"}

					Cull Front

					CGPROGRAM
					#pragma vertex vert
					#pragma fragment frag
					#include "UnityCG.cginc"
					#pragma multi_compile MOVE_NORMAL MOVE_VERTEX
					#pragma shader_feature USE_TANGENT_ON
					#pragma enable_d3d11_debug_symbols

					half _OutlineWidth;
					half4 _OutLineColor;
					float _move_distance;

					struct a2v
				{
						float4 vertex : POSITION;
						float3 normal : NORMAL;
						float2 uv : TEXCOORD0;
						float4 vertColor : COLOR;
						float4 tangent : TANGENT;
					};

					struct v2f
			   {
						float4 pos : SV_POSITION;
						float4 vert_color : COLOR;
					};


					v2f vert(a2v v)
			   {
						v2f o;
						UNITY_INITIALIZE_OUTPUT(v2f, o);
						float4 pos = UnityObjectToClipPos(v.vertex);


		#if MOVE_NORMAL

		#if USE_TANGENT_ON
						float3 viewNormal = mul((float3x3)UNITY_MATRIX_IT_MV, v.tangent.xyz);
		#else
						float3 viewNormal = mul((float3x3)UNITY_MATRIX_IT_MV, v.normal.xyz);
		#endif
						// normalize(TransformViewToProjection(viewNormal.xyz))　这里已经得到NDC空间的normal了，但是因为需要把
						// pos.xy来延伸，正常宽度是/pos.w的，所以这里先乘以pos.w
						float2 offset = normalize(TransformViewToProjection(viewNormal.xyz)).xy;//将法线变换到NDC空间
						float4 nearUpperRight = mul(unity_CameraInvProjection, float4(1, 1, UNITY_NEAR_CLIP_VALUE, _ProjectionParams.y));//将近裁剪面右上角的位置的顶点变换到观察空间
						float aspect = abs(nearUpperRight.y / nearUpperRight.x);//求得屏幕宽高比
						offset.x *= aspect;
		#endif

		#if MOVE_VERTEX
						// other function
						float3 dir = normalize(v.vertex.xyz);
						dir = mul((float3x3)UNITY_MATRIX_IT_MV, dir);
						float2 offset = TransformViewToProjection(dir).xy;
		#endif

						pos.xy += _move_distance * _OutlineWidth * offset * pos.w;
						o.pos = pos;
						o.vert_color = v.vertColor;
						return o;
					}

					half4 frag(v2f i) : SV_TARGET
				{
						return _OutLineColor;
					//return i.vert_color;
				}
				ENDCG
			}		
	}

}
