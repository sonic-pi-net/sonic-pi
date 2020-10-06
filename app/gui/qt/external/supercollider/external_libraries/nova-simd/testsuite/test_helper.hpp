#include <boost/test/floating_point_comparison.hpp>

#include <boost/random.hpp>

template <typename float_type>
float_type randomize_float(void)
{
    static boost::mt19937 rng;
    static boost::uniform_real<float_type> dist(0.1, 1);
    static boost::variate_generator<boost::mt19937&, boost::uniform_real<float_type> >
        gen(rng, dist);
    return gen();
}

template <typename float_type>
float_type randomize_float_slope(void)
{
    static boost::mt19937 rng;
    static boost::uniform_real<float_type> dist(0.0, 0.01);
    static boost::variate_generator<boost::mt19937&, boost::uniform_real<float_type> >
        gen(rng, dist);
    return gen();
}
template <typename float_type>
void randomize_buffer(float_type * buffer, std::size_t size)
{
    for (std::size_t i = 0; i != size; ++i)
        buffer[i] = randomize_float<float_type>();
}

template <typename float_type>
void randomize_buffer(float_type * buffer, std::size_t size, float_type offset)
{
    for (std::size_t i = 0; i != size; ++i)
        buffer[i] = randomize_float<float_type>() + offset;
}

template <typename float_type>
void randomize_buffer(float_type * buffer, std::size_t size, float_type scale, float_type offset)
{
    for (std::size_t i = 0; i != size; ++i)
        buffer[i] = randomize_float<float_type>() * scale + offset;
}


template <typename float_type>
void compare_buffers(const float_type * ref, const float_type * test, std::size_t size, float difference = 2e-7)
{
    for (std::size_t i = 0; i != size; ++i)
        BOOST_REQUIRE_CLOSE_FRACTION( ref[i], test[i], difference );
}

template <typename float_type>
void compare_buffers_relative(const float_type * ref, const float_type * test, std::size_t size)
{
    for (std::size_t i = 0; i != size; ++i)
        BOOST_REQUIRE_CLOSE_FRACTION( ref[i], test[i], 1 );
}
