#ifndef BRDF_INCLUDE
// Upgrade NOTE: excluded shader from DX11, OpenGL ES 2.0 because it uses unsized arrays
#pragma exclude_renderers d3d11 gles
#define BRDF_INCLUDE

/*
    to add a new direct lighting model, you need to fill the struct LightingResult, diffuse and specular lighting.
    LightingVars containes the most vars you need, you can find the definition in common.cginc;
*/

#include "common.cginc"

//  ################   blinnphone lighting

LightingResult direct_blinnphone_lighting(LightingVars data)
{
    LightingResult result;
    result.lighting_diffuse = data.light_color*data.diffuse_color*max(dot(data.N, data.L), 0.0);
    result.lighting_specular = data.light_color*data.f0*pow(max(dot(data.H, data.N), 0.0), 32) ;
    return result;
}

//   ############### cook-torrance pbr lighting

float3 Diffuse_Lambert(float3 DiffuseColor)
{
    return DiffuseColor * (1 / PI);
}

// GGX / Trowbridge-Reitz
// [Walter et al. 2007, "Microfacet models for refraction through rough surfaces"]
float D_GGX( float a2, float NoH )
{
    float d = ( NoH * a2 - NoH ) * NoH + 1;	// 2 mad
    return a2 / ( PI*d*d );					// 4 mul, 1 rcp
}

// Appoximation of joint Smith term for GGX
// [Heitz 2014, "Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs"]
float Vis_SmithJointApprox( float a2, float NoV, float NoL )
{
    float a = sqrt(a2);
    float Vis_SmithV = NoL * ( NoV * ( 1 - a ) + a );
    float Vis_SmithL = NoV * ( NoL * ( 1 - a ) + a );
    return 0.5 * rcp( Vis_SmithV + Vis_SmithL );
}

// [Schlick 1994, "An Inexpensive BRDF Model for Physically-Based Rendering"]
float3 F_Schlick( float3 f0, float VoH )
{
    float Fc = Pow5( 1 - VoH );
    return Fc + f0*(1 - Fc);
}			

float3 SpecularGGX(LightingVars data)
{
    float Roughness = data.roughness;
    float NoH = max(saturate(dot(data.N, data.H)), CHAOS);
    float NoL = max(saturate(dot(data.N, data.L)), CHAOS);
    float NoV = max(saturate(dot(data.N, data.V)), CHAOS);
    float VoH = max(saturate(dot(data.V, data.H)), CHAOS);

    // mtl中存放的是 感知线性粗糙度（为了方便美术调整，所以值为实际值的sqrt)				
    float use_roughness = max(Pow2(Roughness), 0.002);
    float a2 = Pow2(use_roughness);
    //float Energy = EnergyNormalization( a2, Context.VoH, AreaLight );
    float Energy = 1.0;
    
    // Generalized microfacet specular
    float D = D_GGX( a2, NoH ) * Energy;
    float Vis = Vis_SmithJointApprox( a2, NoV, NoL);
    float3 F = F_Schlick(data.f0, VoH);

    return (D * Vis) * F;
}

// Specular term
// HACK: theoretically we should divide diffuseTerm by Pi and not multiply specularTerm!
// BUT 1) that will make shader look significantly darker than Legacy ones
// and 2) on engine side "Non-important" lights have to be divided by Pi too in cases when they are injected into ambient SH
LightingResult isotropy_lighting(LightingVars data)
{
    LightingResult result;

    float NoL = max(dot(data.N, data.L), 0.0);
	// unity的问题，乘以了pi
	result.lighting_diffuse = (data.light_color*NoL) * Diffuse_Lambert(data.diffuse_color)*PI;
    result.lighting_specular = (data.light_color*NoL) * SpecularGGX(data)*PI;
	result.lighting_scatter = float3(0.0, 0.0, 0.0);
    return result;
}

LightingResult subsurface_lighting(LightingVars data)
{
	LightingResult result;

	float NoL = max(dot(data.N, data.L), 0.0);
	result.lighting_diffuse = (data.light_color*NoL) * Diffuse_Lambert(data.diffuse_color)*PI;
	result.lighting_specular = (data.light_color*NoL) * SpecularGGX(data)*PI;

	//加上反向的透射部分
	float trans_dot = pow(saturate(dot(data.V, -data.L)), _sss_power)*_sss_strength*data.opacity;
	result.lighting_scatter = trans_dot * data.sss_color;
	
	/*
	data.opacity = 1.0-_sss_strength;
	float in_scatter = pow(saturate(dot(data.V, -data.L) ), 12.0) * lerp(3.0, 0.1, data.opacity);
	float normal_contribution = dot(data.N, data.H)*data.opacity + 1.0 - data.opacity;
	float back_scatter = normal_contribution / (2.0*PI);
	result.lighting_scatter = data.sss_color*lerp(back_scatter, 1.0, in_scatter);
	*/	
	return result;
}

LightingResult skin_lighting(LightingVars data)
{
	LightingResult result;

	float curvature = saturate( length(fwidth(data.N)) / length(fwidth(data.world_pos)));

	float NoL = max(dot(data.N, data.L), 0.0);
	float2 preinteger_uv;
	preinteger_uv.x = dot(data.N, data.L)*0.5 + 0.5;
	preinteger_uv.y = saturate( curvature * dot(data.light_color, float3(0.22, 0.707, 0.071)) );
	float3 brdf = tex2D(_preinteger_tex, preinteger_uv).rgb;

	result.lighting_diffuse = (data.light_color*lerp(NoL, brdf, data.thickness)) * Diffuse_Lambert(data.diffuse_color)*PI;
	result.lighting_specular = (data.light_color*NoL) * SpecularGGX(data)*PI;

	//加上反向的透射部分，其实 前面nol允许负数取值，就说明了暗部不是一个纯黑，而是一个有微弱红色色彩偏向的值
	//float trans_dot = pow(saturate(dot(data.V, -data.L)), _sss_power)*_sss_strength*data.thickness;
	result.lighting_scatter = float3(0,0,0); // trans_dot * data.sss_color;
	return result;
}

float3 SpecularAnisotropic(LightingVars data)
{
	/*
	//某一种各项异性，带物理性的公式 这里的负号是为什么？？ 神一般的操作啊
	float LoT = dot(-data.L, data.T);
	float VoT = dot(data.V, data.T);
	float shiniess = 1.0f - data.roughness;
	float factor = pow(sqrt(1.0 - LoT * LoT)*sqrt(1.0 - VoT * VoT) + LoT * VoT, shiniess*256.0f)*_anisotropy_intensity;
	return max(0.0, factor)*data.f0;
	*/

	//kajiya的高光公式
	/*
	float shiniess = 1.0f - data.roughness;
	float ToH = dot(data.T, data.H);
	float factor = pow(sqrt(1.0 - ToH * ToH), shiniess*256.0f)*_anisotropy_intensity;
	return max(0.0, factor)*data.f0;
	*/

	
	float at = max(data.roughness*(1.0 + _anisotropy), 0.001f);
	float ab = max(data.roughness*(1.0 - _anisotropy), 0.001f);

	float NoH = max(saturate(dot(data.N, data.H)), CHAOS);
	float NoL = max(saturate(dot(data.N, data.L)), CHAOS);
	float NoV = max(saturate(dot(data.N, data.V)), CHAOS);
	float VoH = max(saturate(dot(data.V, data.H)), CHAOS);

	float ToH = dot(data.T, data.H);
	float BoH = dot(data.B, data.H);

	float ToV = dot(data.T, data.V);
	float BoV = dot(data.B, data.V);
	float ToL = dot(data.T, data.L);
	float BoL = dot(data.B, data.L);

	// D项
	float a2 = at * ab;
	float3 v = float3(ab*ToH, at*BoH, a2*NoH);
	float v2 = dot(v, v);
	float w2 = a2 / v2;
	float D = a2 * w2*w2*(1.0 / PI);

	// V项
	float lambdaV = NoL * length(float3(at * ToV, ab * BoV, NoV));
	float lambdaL = NoV * length(float3(at * ToL, ab * BoL, NoL));
	float Vis = 0.5 / (lambdaV + lambdaL);

	// F项
	float3 F = F_Schlick(data.f0, VoH);

	return (D * Vis) * F * _anisotropy_intensity;
	
}

LightingResult hair_lighting(LightingVars data)
{
	LightingResult result;

	float NoL = max(dot(data.N, data.L), 0.0);
	result.lighting_diffuse = (data.light_color*NoL) * Diffuse_Lambert(data.diffuse_color)*PI;

	// T需要进行jitter
	float jitter = tex2D(_hair_jitter, data.base_vars.uv0).r;
	data.T = data.T + data.N*jitter*_jitter_scale;

	//float3 new_T = tex2D(_hair_tangent, data.base_vars.uv0).rgb*2.0f - float3(1.0f, 1.0f, 1.0f);
	//data.T = data.T*new_T.x + data.B*new_T.y + data.N*new_T.z;

	result.lighting_specular = (data.light_color*NoL) * SpecularAnisotropic(data)*PI;
	result.lighting_scatter = float3(0, 0, 0);
	return result;
}
//////////////////////////////////////////////////////////////////////////////

// max absolute error 9.0x10^-3
// Eberly's polynomial degree 1 - respect bounds
// 4 VGPR, 12 FR (8 FR, 1 QR), 1 scalar
// input [-1, 1] and output [0, PI]
float acosFast(float inX)
{
	float x = abs(inX);
	float res = -0.156583f * x + (0.5 * PI);
	res *= sqrt(1.0f - x);
	return (inX >= 0) ? res : PI - res;
}

// Same cost as acosFast + 1 FR
// Same error
// input [-1, 1] and output [-PI/2, PI/2]
float asinFast(float x)
{
	return (0.5 * PI) - acosFast(x);
}

float Hair_g(float B, float Theta)
{
	return exp(-0.5 * Pow2(Theta) / (B*B)) / (sqrt(2 * PI) * B);
}

float Hair_F(float CosTheta)
{
	const float n = 1.55;
	const float F0 = Pow2((1 - n) / (1 + n));
	return F0 + (1 - F0) * Pow5(1 - CosTheta);
}

// Approximation to HairShadingRef using concepts from the following papers:
// [Marschner et al. 2003, "Light Scattering from Human Hair Fibers"]
// [Pekelis et al. 2015, "A Data-Driven Light Scattering Model for Hair"]
float3 HairShading(LightingVars data)
{
	float Roughness = data.roughness;
	float Area = 0.0f;
	float3 V = data.V;
	float3 L = data.L;
	float3 N = data.T;

	// to prevent NaN with decals
	// OR-18489 HERO: IGGY: RMB on E ability causes blinding hair effect
	// OR-17578 HERO: HAMMER: E causes blinding light on heroes with hair
	float ClampedRoughness = clamp(Roughness, 1 / 255.0f, 1.0f);

	const float VoL = dot(V, L);
	const float SinThetaL = dot(N, L);
	const float SinThetaV = dot(N, V);
	float CosThetaD = cos(0.5 * abs(asinFast(SinThetaV) - asinFast(SinThetaL)));

	//CosThetaD = abs( CosThetaD ) < 0.01 ? 0.01 : CosThetaD;

	const float3 Lp = L - SinThetaL * N;
	const float3 Vp = V - SinThetaV * N;
	const float CosPhi = dot(Lp, Vp) * rsqrt(dot(Lp, Lp) * dot(Vp, Vp) + 1e-4);
	const float CosHalfPhi = sqrt(saturate(0.5 + 0.5 * CosPhi));
	//const float Phi = acosFast( CosPhi );

	float n = 1.55;
	//float n_prime = sqrt( n*n - 1 + Pow2( CosThetaD ) ) / CosThetaD;
	float n_prime = 1.19 / CosThetaD + 0.36 * CosThetaD;

	float Shift = 0.035;
	float Alpha[] =
	{
		-Shift * 2,
		Shift,
		Shift * 4,
	};
	float B[] =
	{
		Area + Pow2(ClampedRoughness),
		Area + Pow2(ClampedRoughness) / 2,
		Area + Pow2(ClampedRoughness) * 2,
	};

	float3 S = 0;
	float Backlit = 1.0f;

	// R
	if (1)
	{
		const float sa = sin(Alpha[0]);
		const float ca = cos(Alpha[0]);
		float Shift = 2 * sa* (ca * CosHalfPhi * sqrt(1 - SinThetaV * SinThetaV) + sa * SinThetaV);

		float Mp = Hair_g(B[0] * sqrt(2.0) * CosHalfPhi, SinThetaL + SinThetaV - Shift);
		float Np = 0.25 * CosHalfPhi;
		float Fp = Hair_F(sqrt(saturate(0.5 + 0.5 * VoL)));
		//S += Mp * Np * Fp * (data.f0 * 2);// *(1.0f - saturate(-VoL));
		S += (Mp * Np) * (Fp * lerp(1, Backlit, saturate(-VoL)));
	}

	// TT
	if (1)
	{
		float Mp = Hair_g(B[1], SinThetaL + SinThetaV - Alpha[1]);

		float a = 1 / n_prime;
		//float h = CosHalfPhi * rsqrt( 1 + a*a - 2*a * sqrt( 0.5 - 0.5 * CosPhi ) );
		//float h = CosHalfPhi * ( ( 1 - Pow2( CosHalfPhi ) ) * a + 1 );
		float h = CosHalfPhi * (1 + a * (0.6 - 0.8 * CosPhi));
		//float h = 0.4;
		//float yi = asinFast(h);
		//float yt = asinFast(h / n_prime);

		float f = Hair_F(CosThetaD * sqrt(saturate(1 - h * h)));
		float Fp = Pow2(1 - f);
		//float3 Tp = pow( GBuffer.BaseColor, 0.5 * ( 1 + cos(2*yt) ) / CosThetaD );
		//float3 Tp = pow( GBuffer.BaseColor, 0.5 * cos(yt) / CosThetaD );
		float3 Tp = pow(data.diffuse_color, 0.5 * sqrt(1 - Pow2(h * a)) / CosThetaD);

		//float t = asin( 1 / n_prime );
		//float d = ( sqrt(2) - t ) / ( 1 - t );
		//float s = -0.5 * PI * (1 - 1 / n_prime) * log( 2*d - 1 - 2 * sqrt( d * (d - 1) ) );
		//float s = 0.35;
		//float Np = exp( (Phi - PI) / s ) / ( s * Pow2( 1 + exp( (Phi - PI) / s ) ) );
		//float Np = 0.71 * exp( -1.65 * Pow2(Phi - PI) );
		float Np = exp(-3.65 * CosPhi - 3.98);

		S += Mp * Np * Fp * Tp;
	}

	// TRT
	if (1)
	{
		float Mp = Hair_g(B[2], SinThetaL + SinThetaV - Alpha[2]);

		//float h = 0.75;
		float f = Hair_F(CosThetaD * 0.5);
		float Fp = Pow2(1 - f) * f;
		//float3 Tp = pow( GBuffer.BaseColor, 1.6 / CosThetaD );
		float3 Tp = pow(data.diffuse_color, 0.8 / CosThetaD);

		//float s = 0.15;
		//float Np = 0.75 * exp( Phi / s ) / ( s * Pow2( 1 + exp( Phi / s ) ) );
		float Np = exp(17 * CosPhi - 16.78);

		S += Mp * Np * Fp * Tp;
	}

	/*
	if (1)
	{
		// Use soft Kajiya Kay diffuse attenuation
		float KajiyaDiffuse = 1 - abs(dot(N, L));

		float3 FakeNormal = normalize(V - N * dot(V, N));
		//N = normalize( DiffuseN + FakeNormal * 2 );
		N = FakeNormal;

		// Hack approximation for multiple scattering.
		float Wrap = 1;
		float NoL = saturate((dot(N, L) + Wrap) / Square(1 + Wrap));
		float DiffuseScatter = (1 / PI) * lerp(NoL, KajiyaDiffuse, 0.33) * GBuffer.Metallic;
		float Luma = Luminance(GBuffer.BaseColor);
		float3 ScatterTint = pow(GBuffer.BaseColor / Luma, 1 - Shadow);
		S += sqrt(GBuffer.BaseColor) * DiffuseScatter * ScatterTint;
	}
	*/

	S = -min(-S, 0.0);

	return S;
}

float3 HairDiffuseKajiyaUE(LightingVars data) {
	float3 N = data.T;
	float3 L = data.L;
	float3 V = data.V;
	float Shadow = data.shadow;

	float3 S = 0;
	float KajiyaDiffuse = 1 - abs(dot(N, L));

	float3 FakeNormal = normalize(V - N * dot(V, N));
	N = FakeNormal;

	// Hack approximation for multiple scattering.
	float Wrap = 1;
	float NoL = saturate((dot(N, L) + Wrap) / ((1 + Wrap)*(1+Wrap)));
	float DiffuseScatter = (1 / PI) * lerp(NoL, KajiyaDiffuse, 0.33);// *s.Metallic;
	float Luma = Luminance(data.diffuse_color);
	float3 ScatterTint = pow(data.diffuse_color / Luma, 1 - Shadow);
	S = sqrt(data.diffuse_color) * DiffuseScatter * ScatterTint;
	return S;
}

LightingResult direct_lighting(LightingVars data)
{
	#if _LIGHTING_TYPE_DEFAULT
		return isotropy_lighting(data);
	#elif _LIGHTING_TYPE_SUBSURFACE
		return subsurface_lighting(data);
	#elif _LIGHTING_TYPE_SKIN
		return skin_lighting(data);
	#elif _LIGHTING_TYPE_HAIR
		return hair_lighting(data);
    #else
		return isotropy_lighting(data);
	#endif
}
#endif