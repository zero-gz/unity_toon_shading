Shader "Custom/Bronya"
{
    Properties
    {
        _Color ("Color", Color) = (1,1,1,1)
        _MainTex ("Albedo (RGB)", 2D) = "white" {}
        _Glossiness ("Smoothness Scale", Range(0,1)) = 0.5
        _Metallic ("Metallic", Range(0,1)) = 0.0

		_LightMap("Light Map", 2D) = "white" {}

		_DividSharpness("Sharpness of Divide Line", Range(0.2,5)) = 1.0

		_DividLineSpec("DividLine of Specular", Range(0.5, 1.0)) = 0.8
		_DividLineM("DividLine of Middle", Range(-0.5, 0.8)) = 0.0
		_DividLineD("DividLine of Dark", Range(-1.0, 0.0)) = -0.5

		_diffuseBright("diffuse Brightness", Range(0.0,2.0)) = 1.0
		_AOWeight("Weight of Ambient Occlusion", Range(0.0,2.0)) = 1.0

		_ShadowAttWeight("Weight of shadow atten", range(0.0, 0.5)) = 0.3

		_DarkFaceColor("Color of Dark Face", Color) = (1.0, 1.0, 1.0, 1.0)
		_DeepDarkColor("Color of Deep Dark Face", Color) = (1.0, 1.0, 1.0, 1.0)

		_FresnelEff("Fresnel Effect", Range(0, 1)) = 0.5
		_FresnelColor("Fresnel Color", Color) = (1,1,1,1)

		_OutlineWidth("Outline Width", Range(0, 0.5)) = 0.024
		_OutlineColor("Outline Color", Color) = (0.5,0.5,0.5,1)
    }

	CGINCLUDE
	#include "UnityCG.cginc"

	float D_GGX(float a2, float NoH) {
		float d = (NoH * a2 - NoH) * NoH + 1;
		return a2 / (3.14159 * d * d);
	}

	float sigmoid(float x, float center, float sharp) {
		float s;
		s = 1 / (1 + pow(100000, (-3 * sharp * (x - center))));
		return s;
	}

	float Pow2(float x) {
		return x * x;
	}

	float ndc2Normal(float x) {
		return x * 0.5 + 0.5;
	}

	float Normal2ndc(float x) {
		return x * 2.0 - 1.0;
	}

	float warp(float x, float w) {
		return (x + w) / (1 + w);
	}

	float3 warp(float3 x, float3 w) {
		return (x + w) / (1 + w);
	}

	float Pow3(float x) {
		return x * x* x;
	}

	float Pow5(float x) {
		return x * x* x* x* x;
	}

	float3 Fresnel_schlick(float VoN, float3 rF0) {
		return rF0 + (1 - rF0) * Pow5(1 - VoN);
	}

	float3 Fresnel_extend(float VoN, float3 rF0) {
		return rF0 + (1 - rF0) * Pow3(1 - VoN);
	}
	ENDCG

    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 200

        CGPROGRAM
        // Physically based Standard lighting model, and enable shadows on all light types
        #pragma surface surf Toon fullforwardshadows

        // Use shader model 3.0 target, to get nicer looking lighting
        #pragma target 3.0

        sampler2D _MainTex;
		sampler2D _LightMap;
		float4 _LightMap_TexelSize;

		struct Input
		{
			float2 uv_MainTex;
			float2 uv_LightMap;
		};

		struct ToonSurfaceOutput
		{
			fixed3 Albedo;  // diffuse color
			fixed3 Normal;  // tangent space normal, if written
			fixed3 Emission;
			half Specular;  // specular power in 0..1 range
			fixed Gloss;    // specular intensity
			fixed Alpha;    // alpha for transparencies

			fixed3 diffColor;

			half specIntensity;
			half smoothMap;
			half AO;
		};

        half _Glossiness;
        half _Metallic;
        fixed4 _Color;
		half _DividLineSpec;
		half _ShadowAttWeight;
		half _DividSharpness;
		half _DividLineM;
		half _DividLineD;
		half _diffuseBright;
		half _AOWeight;
		fixed4 _DarkFaceColor;
		fixed4 _DeepDarkColor;
		half _FresnelEff;
		fixed4 _FresnelColor;
        // Add instancing support for this shader. You need to check 'Enable Instancing' on materials that use the shader.
        // See https://docs.unity3d.com/Manual/GPUInstancing.html for more information about instancing.
        // #pragma instancing_options assumeuniformscaling
        UNITY_INSTANCING_BUFFER_START(Props)
            // put more per-instance properties here
        UNITY_INSTANCING_BUFFER_END(Props)

		half4 LightingToon(ToonSurfaceOutput s, half3 lightDir, half3 viewDir, half atten) {
			// Array
			half3 nNormal = normalize(s.Normal);
			half3 HDir = normalize(lightDir + viewDir);

			half NoL = dot(nNormal, lightDir);
			half NoH = dot(nNormal, HDir);
			half NoV = dot(nNormal, viewDir);
			half VoL = dot(viewDir, lightDir);
			half VoH = dot(viewDir, HDir);

			half roughness = 0.95 - 0.95 * (s.smoothMap * _Glossiness);
			half _BoundSharp = 9.5 * Pow2(roughness - 1) + 0.5;
			//----------------------------------------------------
			// fresnel
			//----------------------------------------------------
			half3 fresnel = Fresnel_extend(NoV, float3(0.1, 0.1, 0.1));
			half3 fresnelResult = _FresnelEff * fresnel * (1 - VoL) / 2;

			//--------------------------------------------
			// Specular
			//--------------------------------------------

			half NDF0 = D_GGX(roughness * roughness, 1);
			half NDF_HBound = NDF0 * _DividLineSpec;

			half NDF = D_GGX(roughness * roughness, NoH) + _ShadowAttWeight * (atten - 1);
			half specularWin = sigmoid(NDF, NDF_HBound, _BoundSharp * _DividSharpness);

			half SpecWeight = specularWin * (NDF0 + NDF_HBound) / 2 * s.specIntensity; //optional

			//----------------------------------------------------
			// diffuse
			//--------------------------------------------
			half Lambert = NoL + _AOWeight * Normal2ndc(s.AO) + _ShadowAttWeight * (atten - 1);

			half MidSig = sigmoid(Lambert, _DividLineM, _BoundSharp * _DividSharpness);
			half DarkSig = sigmoid(Lambert, _DividLineD, _BoundSharp * _DividSharpness);

			half MidLWin = MidSig;
			half MidDWin = DarkSig - MidSig;
			half DarkWin = 1 - DarkSig;

			half3 diffuseWeight = (MidLWin * (1 + ndc2Normal(_DividLineM)) / 2).xxx;
			diffuseWeight += (MidDWin * (ndc2Normal(_DividLineM) + ndc2Normal(_DividLineD)) / 2).xxx * _DarkFaceColor.rgb * 3 / (_DarkFaceColor.r + _DarkFaceColor.g + _DarkFaceColor.b);
			diffuseWeight += (DarkWin * (ndc2Normal(_DividLineD))).xxx * _DeepDarkColor.rgb * 3 / (_DeepDarkColor.r + _DeepDarkColor.g + _DeepDarkColor.b);
			diffuseWeight = warp(diffuseWeight, _diffuseBright.xxx);

			half3 lightResult = SpecWeight * _LightColor0.rgb + (1 - SpecWeight) * diffuseWeight * s.diffColor.rgb + fresnelResult * _FresnelColor.rgb;

			return half4(lightResult.rgb, 1.0);
		}

        void surf (Input IN, inout ToonSurfaceOutput o)
        {
			// Albedo comes from a texture tinted by color
			fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
			o.diffColor = c.rgb;
			o.Albedo = 0.2 * c.rgb;

			fixed4 ilm = tex2D(_LightMap, IN.uv_LightMap);

			
			//-------------------------------------------
			// blur
			float2 tmpuv1 = IN.uv_LightMap + _LightMap_TexelSize.xy;
			float2 tmpuv2 = IN.uv_LightMap - _LightMap_TexelSize.xy;
			float2 tmpuv3 = IN.uv_LightMap;
			tmpuv3.x += _LightMap_TexelSize.x;
			tmpuv3.y -= _LightMap_TexelSize.y;
			float2 tmpuv4 = IN.uv_LightMap;
			tmpuv4.x -= _LightMap_TexelSize.x;
			tmpuv4.y += _LightMap_TexelSize.y;

			fixed4 ilm1 = tex2D(_LightMap, tmpuv1);
			fixed4 ilm2 = tex2D(_LightMap, tmpuv2);
			fixed4 ilm3 = tex2D(_LightMap, tmpuv3);
			fixed4 ilm4 = tex2D(_LightMap, tmpuv4);

			//ilm = 0.4 * ilm + 0.15 * (ilm1 + ilm2 + ilm3 + ilm4);
			ilm = 0.2 * (ilm + ilm1 + ilm2 + ilm3 + ilm4);
			//---------------------------------------
			

			o.smoothMap = ilm.r;

			o.specIntensity = ilm.b;
			o.AO = ilm.g;

			o.Alpha = c.a;
        }
        ENDCG

		Pass{
		Name "OUTLINE"
		Tags{ "LightMode" = "Always" }
		Cull Front
		ZWrite On
		ColorMask RGB
		Blend SrcAlpha OneMinusSrcAlpha

		CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag

			#include "UnityCG.cginc"
			struct appdata {
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 texCoord : TEXCOORD0;

			};

			struct v2f {
				float4 pos : SV_POSITION;
				float4 color : COLOR;
				float4 tex : TEXCOORD0;
			};

			uniform half _OutlineWidth;
			sampler2D _MainTex;
			fixed4 _OutlineColor;

			v2f vert(appdata v) {
				// just make a copy of incoming vertex data but scaled according to normal direction
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				float3 norm = mul((float3x3)UNITY_MATRIX_IT_MV, v.normal);
				float2 extendDir = normalize(TransformViewToProjection(norm.xy));

				_OutlineWidth = _OutlineWidth;

				o.pos.xy += extendDir * (o.pos.w * _OutlineWidth * 0.1);

				o.tex = v.texCoord;

				o.color = half4(0,0,0,1);
				return o;
			}

			half4 frag(v2f i) :COLOR{
				fixed4 c = tex2D(_MainTex, i.tex);

				return half4(c.rgb* _OutlineColor.rgb, 1.0);
			}

			ENDCG

		}//Pass
    }
    FallBack "Diffuse"
}