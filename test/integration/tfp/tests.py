from eight_schools import eight_schools_ncp_model
import eight_schools_data
from test_unbounded_cont import test_unbounded_cont_model
import test_unbounded_cont_data
from test_positive_cont import test_positive_cont_model
import test_positive_cont_data

from stan import stan, merge_chains
import unittest
import numpy as np

def significant_digit(x):
    return float("{:.1}".format(x))

def significant_mean(x):
    return significant_digit(np.mean(x))


class TestModels(unittest.TestCase):

    def test_eight_schools(self):
        target_dist = eight_schools_ncp_model(**eight_schools_data.data)
        mcmc_trace, _ = stan(target_dist)
        mu, tau, theta_tilde = [merge_chains(x) for x in mcmc_trace]

        self.assertAlmostEqual(significant_mean(mu), 4, delta=2)
        self.assertAlmostEqual(significant_mean(tau), 3, delta=2)
        self.assertAlmostEqual(significant_mean(theta_tilde), 0.08,
                               delta=0.1)
    
    def test_unbounded_cont(self):
        target_dist = test_unbounded_cont_model(**test_unbounded_cont_data.data)
        mcmc_trace, _ = stan(target_dist)
        loc_normal, scale_normal, loc_cauchy, scale_cauchy, \
            loc_gumbel, scale_gumbel, loc_student_t, scale_student_t,\
                 nu, loc_laplace, scale_laplace = [merge_chains(x) for x in mcmc_trace]

        self.assertAlmostEqual(significant_mean(loc_normal), -3, delta=1)
        self.assertAlmostEqual(significant_mean(scale_normal), 5, delta=1)

        self.assertAlmostEqual(significant_mean(loc_cauchy), -3, delta=1)
        self.assertAlmostEqual(significant_mean(scale_cauchy), 3, delta=1)

        self.assertAlmostEqual(significant_mean(loc_gumbel), -6, delta=1)
        self.assertAlmostEqual(significant_mean(scale_gumbel), 6, delta=1)

        self.assertAlmostEqual(significant_mean(loc_student_t), -3, delta=1)
        self.assertAlmostEqual(significant_mean(scale_student_t), 5, delta=1)
        self.assertAlmostEqual(significant_mean(nu), 25, delta=6)

        self.assertAlmostEqual(significant_mean(loc_laplace), -4, delta=1)
        self.assertAlmostEqual(significant_mean(scale_laplace), 4, delta=1)

    def test_positive_cont(self):
        target_dist = test_positive_cont_model(**test_positive_cont_data.data)
        mcmc_trace, _ = stan(target_dist)
        loc_lognormal, scale_lognormal, chi_square_nu, exp_rate, \
            gamma_alpha, gamma_beta, inv_gamma_alpha, inv_gamma_beta = [merge_chains(x) for x in mcmc_trace]

        self.assertAlmostEqual(significant_mean(loc_lognormal), 3, delta=1)
        self.assertAlmostEqual(significant_mean(scale_lognormal), 0.3, delta=0.1)

        self.assertAlmostEqual(significant_mean(chi_square_nu), 20, delta=3)

        self.assertAlmostEqual(significant_mean(exp_rate), 0.05, delta=0.01)

        self.assertAlmostEqual(significant_mean(gamma_alpha), 10, delta=2)
        self.assertAlmostEqual(significant_mean(gamma_beta), 0.5, delta=0.1)

        self.assertAlmostEqual(significant_mean(inv_gamma_alpha), 2, delta=1)
        self.assertAlmostEqual(significant_mean(inv_gamma_beta), 30, delta=3)

if __name__ == '__main__':
    unittest.main()
