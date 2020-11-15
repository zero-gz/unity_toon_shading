#ifndef COMMON_INCLUDE
#define COMMON_INCLUDE

#define PI 3.1415926
#define BLACK_COLOR float3(0.0, 0.0, 0.0)
#define WHITE_COLOR float3(1.0, 1.0, 1.0)
#define CHAOS 0.000001	

struct appdata
{
	float4 vertex : POSITION;
	float2 uv : TEXCOORD0;
	float3 normal : NORMAL;
	float4 tangent : TANGENT;
	#ifdef LIGHTMAP_ON
		float2 uv1 : TEXCOORD1;
	#endif

	#ifdef DYNAMICLIGHTMAP_ON
		float2 uv2 : TEXCOORD2;
	#endif
};

struct v2f
{
	float2 uv : TEXCOORD0;
	float4 pos : SV_POSITION;
	float3 world_pos: TEXCOORD1;
	float3 world_normal: TEXCOORD2;
	float3 world_tangent: TEXCOORD3;
	float3 world_binnormal: TEXCOORD4;
	#if defined(LIGHTMAP_ON) || defined(DYNAMICLIGHTMAP_ON)
		float4 lightmap_uv: TEXCOORD5;
	#endif
	float4 screen_pos:TEXCOORD6;

	SHADOW_COORDS(7)
};

sampler2D _albedo_tex;
sampler2D _normal_tex;
sampler2D _mix_tex;
sampler2D _preinteger_tex;
sampler2D _sss_tex;
sampler2D _emissive_tex;

float _roughness;
float _metallic;
float3 _emissive;
float4 _sss_color;
float _sss_power;

struct BaseVars {
    float3 pos;
    float2 uv0;
};

struct LightingVars {
    float3 T;
    float3 B;
    float3 N;
    float3 V;
    float3 L;
    float3 H;

    float3 albedo;
    float3 diffuse_color;
    float3 f0;
    float roughness;
    float metallic;
	float opacity;

    float3 light_color;

    #if defined(LIGHTMAP_ON) || defined(DYNAMICLIGHTMAP_ON)
        float4 lightmap_uv;
    #endif

    float3 world_pos;
    float occlusion; //遮蔽 全局光，包括indrect diffuse和specular
    float shadow; // 遮蔽实时灯光

    BaseVars base_vars; //v2f中的一些关键变量

	// subsurface color
	float3 sss_color;
	// skin
	float thickness;
	float curvature;

	float4 pos;
};

struct MaterialVars {
    float3 albedo;
    float3 normal;
    float roughness;
    float metallic;
    float3 emissive;
    float opacity;
    float occlusion;

	float3 sss_color;
	float thickness;
	float curvature;
};

struct LightingResult{
    float3 lighting_diffuse;
    float3 lighting_specular;
	float3 lighting_scatter;
};

float Pow2(float c)
{
    return c*c;
}

float3 gamma_correct_began(float3 input_color)
{
    return input_color*input_color;
}		

float3 gamma_correct_end(float3 input_color)
{
    return sqrt(input_color);
}

#endif