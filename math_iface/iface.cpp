#include <stan/math/rev/mat.hpp>
extern "C" {
    double lognormal_ccdf_log(double a,double b, double c) {
        try {
            return stan::math::lognormal_ccdf_log(a,b,c);
        } catch(...) {
            return -999;
        }
    }
}
