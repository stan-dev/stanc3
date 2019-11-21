from eight_schools import eight_schools_ncp_model
import eight_schools_data

from test_unbounded_cont import test_unbounded_cont_model
import test_unbounded_cont_data
import test_unbounded_cont_fit 

from test_positive_cont import test_positive_cont_model
import test_positive_cont_data
import test_positive_cont_fit

from stan import stan, merge_chains
import unittest
import numpy as np
import tensorflow as tf

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
    
    def _compare_lp(self, target_dist, stan_fit):
        # get stan samples and lp
        stan_samples = stan_fit[:,:-1]
        stan_lp = stan_fit[:,-1]

        # compute lp according to tfp model
        tfp_lp = target_dist.log_prob(tf.cast(stan_samples,tf.float64)).numpy()

        # correct for differences in log_det calculations
        bijectors = target_dist.parameter_bijectors()
        log_det_corrections = [bijector.inverse_log_det_jacobian(param,0).numpy() for bijector, param in zip(bijectors, stan_samples.T)]

        # if the bijector is tfb.Identity, its log_det_correction is an empty vector, we get rid of these so we can stack and sum
        log_det_corrections = np.stack([corr for corr in log_det_corrections if corr.size>1]).sum(0)
        self.assertTrue(np.allclose(tfp_lp-log_det_corrections, stan_lp))

    def test_unbounded_cont(self):
        target_dist = test_unbounded_cont_model(**test_unbounded_cont_data.data)
        stan_fit = test_unbounded_cont_fit.data
        self._compare_lp(target_dist, stan_fit)

    def test_positive_cont(self):
        target_dist = test_positive_cont_model(**test_positive_cont_data.data)
        stan_fit = test_positive_cont_fit.data
        self._compare_lp(target_dist, stan_fit)


        # mcmc_trace, _ = stan(target_dist)
        # loc_lognormal, scale_lognormal, chi_square_nu, exp_rate, \
        #     gamma_alpha, gamma_beta, inv_gamma_alpha, inv_gamma_beta = [merge_chains(x) for x in mcmc_trace]

        # self.assertAlmostEqual(significant_mean(loc_lognormal), 3, delta=1)
        # self.assertAlmostEqual(significant_mean(scale_lognormal), 0.3, delta=0.1)

        # self.assertAlmostEqual(significant_mean(chi_square_nu), 20, delta=3)

        # self.assertAlmostEqual(significant_mean(exp_rate), 0.05, delta=0.01)

        # self.assertAlmostEqual(significant_mean(gamma_alpha), 10, delta=2)
        # self.assertAlmostEqual(significant_mean(gamma_beta), 0.5, delta=0.1)

        # self.assertAlmostEqual(significant_mean(inv_gamma_alpha), 2, delta=1)
        # self.assertAlmostEqual(significant_mean(inv_gamma_beta), 30, delta=3)

if __name__ == '__main__':
    unittest.main()
