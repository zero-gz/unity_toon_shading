#ifndef GI_LIGHTING_INCLUDE
#define GI_LIGHTING_INCLUDE

/*
    the gi_lighting is the mix functions.
    static object = baked_lightmap + realtime_lightmap + ibl(specular)
    dynamic object = sh_lighting + ibl(specular)

    ibl(specular) = (unity samplelod mipmaps)*(black ops II, curve fitting)
*/

#include "common.cginc"

void init_result(inout LightingResult result)
{
    result.lighting_diffuse = float3(0.0, 0.0, 0.0);
    result.lighting_specular = float3(0.0, 0.0, 0.0);
	result.lighting_scatter = float3(0.0, 0.0, 0.0);
}

float3 ibl_lighting_diffuse(LightingVars data)
{
    return data.diffuse_color * ShadeSH9(float4(data.N, 1.0));
}

// Env BRDF Approx
float3 env_approx(LightingVars data)
{
    float NoV = max(saturate(dot(data.N, data.V)), CHAOS);

    float4 C0 = float4(-1.000f, -0.0275f, -0.572f,  0.022f);
    float4 C1 = float4(1.000f,  0.0425f,  1.040f, -0.040f);
    float2 C2 = float2(-1.040f,  1.040f);
    float4 r = C0 * data.roughness + C1;
    float a = min(r.x * r.x, exp2(-9.28f * NoV)) * r.x + r.y;
    float2 ab = C2 * a + r.zw;

    return data.f0*ab.x + float3(ab.y, ab.y, ab.y);
}			

float3 ibl_lighting_specular(LightingVars data)
{
    // ibl specular part1
    float mip_roughness = data.roughness * (1.7 - 0.7 * data.roughness);
    float3 reflectVec = reflect(-data.V, data.N);

    half mip = mip_roughness * UNITY_SPECCUBE_LOD_STEPS;
    half4 rgbm = UNITY_SAMPLE_TEXCUBE_LOD(unity_SpecCube0, reflectVec, mip);

    float3 iblSpecular = DecodeHDR(rgbm, unity_SpecCube0_HDR);

    // ibl specular part2
    float3 brdf_factor = env_approx(data);

    return iblSpecular*brdf_factor;
}

float3 lightmap_lighting_diffuse(LightingVars data)
{
    float3 lightmap_baked = float3(0.0, 0.0, 0.0);
    float3 lightmap_realtime = float3(0.0, 0.0, 0.0);
    #if defined(LIGHTMAP_ON)
        // Baked lightmaps
        half4 bakedColorTex = UNITY_SAMPLE_TEX2D(unity_Lightmap, data.lightmap_uv.xy);
        half3 bakedColor = DecodeLightmap(bakedColorTex);

        #ifdef DIRLIGHTMAP_COMBINED
            fixed4 bakedDirTex = UNITY_SAMPLE_TEX2D_SAMPLER (unity_LightmapInd, unity_Lightmap, data.lightmap_uv.xy);
            lightmap_baked += DecodeDirectionalLightmap (bakedColor, bakedDirTex, data.N);
        #else
            lightmap_baked += bakedColor;	
        #endif
    #endif

    #ifdef DYNAMICLIGHTMAP_ON
        // Dynamic lightmaps
        fixed4 realtimeColorTex = UNITY_SAMPLE_TEX2D(unity_DynamicLightmap, data.lightmap_uv.zw);
        half3 realtimeColor = DecodeRealtimeLightmap (realtimeColorTex);

        #ifdef DIRLIGHTMAP_COMBINED
            half4 realtimeDirTex = UNITY_SAMPLE_TEX2D_SAMPLER(unity_DynamicDirectionality, unity_DynamicLightmap, data.lightmap_uv.zw);
            lightmap_realtime += DecodeDirectionalLightmap (realtimeColor, realtimeDirTex, data.N);
        #else
            lightmap_realtime += realtimeColor;
        #endif
    #endif

    return (lightmap_baked + lightmap_realtime)*data.diffuse_color;
}


LightingResult gi_isotropy_lighting(LightingVars data)
{
    LightingResult result;
    init_result(result);
    
    #ifdef LIGHTPROBE_SH
        result.lighting_diffuse += ibl_lighting_diffuse(data);
    #endif

    #ifdef UNITY_SPECCUBE_BOX_PROJECTION
        result.lighting_specular += ibl_lighting_specular(data);
    #endif	

    #if defined(LIGHTMAP_ON) || defined(DYNAMICLIGHTMAP_ON)
        result.lighting_diffuse += lightmap_lighting_diffuse(data);
    #endif

    return result;
}

LightingResult gi_lighting(LightingVars data)
{
	return gi_isotropy_lighting(data);
}

#endif