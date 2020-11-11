Shader "Unlit/Ouline"
{
	Properties
	{
		_MainTex("MainTex", 2D) = "white" {}
		_MainColor("Main Color", Color) = (1,1,1)
		_ShadowColor("Shadow Color", Color) = (0.7, 0.7, 0.8)
		_ShadowRange("Shadow Range", Range(0, 1)) = 0.5
		_ShadowSmooth("Shadow Smooth", Range(0, 1)) = 0.2

		_tex_ctrl_specular_intensity("ilm texture r channel", Range(0, 1)) = 1
		_tex_ctrl_threshold("ilm texture g channel", Range(0, 1)) = 0.5
		_tex_ctrl_specular_mask("ilm texture b channel", Range(0, 1)) = 1

		[Space(20)]
		[Toggle]ENABLE_DIFFUSE("ENABLE_DIFFUSE", Float) = 1
		[Toggle]ENABLE_SPECULAR("ENABLE_SPECULAR", Float) = 1
		_SpecularColor("Specular Color", Color) = (1,1,1)
		_SpecularRange("Specular Range",  Range(0, 1)) = 0.9
		_SpecularMulti("Specular Multi", Range(0, 2)) = 0.1
		_SpecularGloss("Sprecular Gloss", Range(0.001, 10)) = 0.2		

		[Space(10)]
		[KeywordEnum(NORMAL, VERTEX)] MOVE("seconad pass move mode", Float) = 0
		[Toggle]USE_TANGENT("USE_TANGENT", Float) = 0
		_OutlineWidth("Outline Width", Range(0.01, 2)) = 0.24
		_OutLineColor("OutLine Color", Color) = (0.5,0.5,0.5,1)
		_move_distance("Move distance", Range(0, 0.3)) = 0.01

		[Space(10)]
		[Toggle]ENABLE_RIMLIGHT("ENABLE_RIMLIGHT", Float) = 0
		_RimColor("Rim Color", Color) = (0,0,0,1)
		_RimMin("Rim min", Range(0,1)) = 0
		_RimMax("Rim max", Range(0, 1)) = 1
		_RimSmooth("Rim smooth", Range(0, 1)) = 1
	}
		SubShader
	{
		Tags { "RenderType" = "Opaque" }

		pass
		{
		   Tags {"LightMode" = "ForwardBase"}

			Cull Back

			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag

			   #pragma shader_feature ENABLE_DIFFUSE_ON
			   #pragma shader_feature ENABLE_SPECULAR_ON
			   #pragma shader_feature ENABLE_RIMLIGHT_ON
			   #pragma enable_d3d11_debug_symbols

		#include "UnityCG.cginc"
		#include "Lighting.cginc"
			#include "AutoLight.cginc"

			sampler2D _MainTex;
		float4 _MainTex_ST;
			half3 _MainColor;
		half3 _ShadowColor;
			half _ShadowRange;
			float _ShadowSmooth;
			float _tex_ctrl_specular_intensity;
			float _tex_ctrl_threshold;
			float _tex_ctrl_specular_mask;


			half3 _SpecularColor;
			half _SpecularRange;
			half _SpecularMulti;
			half _SpecularGloss;

			float4 _RimColor;
			float _RimMin;
			float _RimMax;
			float _RimSmooth;


			struct a2v
	   {
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			struct v2f
	   {
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
				float3 worldNormal : TEXCOORD1;
		float3 worldPos : TEXCOORD2;
			};


			v2f vert(a2v v)
	  {
				v2f o;
				UNITY_INITIALIZE_OUTPUT(v2f, o);
				o.uv = TRANSFORM_TEX(v.uv, _MainTex);
				o.worldNormal = UnityObjectToWorldNormal(v.normal);
				o.worldPos = mul(unity_ObjectToWorld, v.vertex).xyz;
				o.pos = UnityObjectToClipPos(v.vertex);
				return o;
			}

			half4 frag(v2f i) : SV_TARGET
	   {
				half4 col = 0;
				half4 mainTex = tex2D(_MainTex, i.uv);
				half3 viewDir = normalize(_WorldSpaceCameraPos.xyz - i.worldPos.xyz);
				half3 worldNormal = normalize(i.worldNormal);
				half3 worldLightDir = normalize(_WorldSpaceLightPos0.xyz);
				half halfLambert = dot(worldNormal, worldLightDir) * 0.5 + 0.5;
				half threshold = saturate((halfLambert + _tex_ctrl_threshold)*0.5 - _ShadowRange);
				half ramp = smoothstep(0, _ShadowSmooth, threshold);
				//use color ramp texture
				//half ramp = tex2D(_rampTex, float2(threshold, 0.5)).r;
				half3 diffuse = lerp(_ShadowColor, _MainColor, ramp);
				diffuse *= mainTex;

				col.rgb = 0;
#ifdef ENABLE_DIFFUSE_ON
				col.rgb += _LightColor0 * diffuse;
#endif

#ifdef ENABLE_SPECULAR_ON
				half3 specular = 0;
				half3 halfDir = normalize(worldLightDir + viewDir);
				half NdotH = max(0, dot(worldNormal, halfDir));
				half SpecularSize = pow(NdotH, _SpecularGloss);
				if (SpecularSize >= 1 - _tex_ctrl_specular_mask * _SpecularRange)
				{
					specular = _SpecularMulti * _tex_ctrl_specular_intensity * _SpecularColor;
				}
				col.rgb += _LightColor0 * specular;
#endif

#ifdef ENABLE_RIMLIGHT_ON
				// 需要加上一个边缘光mask
				half f = 1.0 - saturate(dot(viewDir, worldNormal));
				half rim = smoothstep(_RimMin, _RimMax, f);
				rim = smoothstep(0, _RimSmooth, rim);
				half3 rimColor = rim * _RimColor.rgb *  _RimColor.a;

				col.rgb += rimColor;
#endif

				return col;
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