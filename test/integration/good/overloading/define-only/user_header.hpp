#include <stan/model/model_header.hpp>
namespace define_only_model_namespace
{

    using stan::model::model_base_crtp;
    using namespace stan::math;

    template <typename T0__>
    stan::promote_args_t<T0__>
    bar(const T0__ &x, std::ostream *pstream__)
    {
        return 1.0;
    }
    double bar(const int &x, std::ostream *pstream__)
    {

        return 2.0;
    }

}
