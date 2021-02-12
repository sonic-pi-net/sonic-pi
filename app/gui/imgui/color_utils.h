#pragma once

inline ImVec4 blend(const ImVec4& left, const ImVec4& right, float a)
{
    float oneMinus = 1.0f - a;
    return { left.x * oneMinus + right.x * a,
        left.y * oneMinus + right.y * a,
        left.z * oneMinus + right.z * a,
        left.w * oneMinus + right.w * a };
}
