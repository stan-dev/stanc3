from eight_schools import eight_schools_ncp_model
import eight_schools_data

from normal_lub import normal_lub_model
import normal_lub_data

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

    def test_normal_lub(self):
        target_dist = normal_lub_model(**normal_lub_data.data)
        mcmc_trace, _ = stan(target_dist)
        theta_lub, theta_ub, theta_lb = [merge_chains(x) for x in mcmc_trace]

        self.assertTrue(np.all(theta_lub<=3))
        self.assertTrue(np.all(theta_lub>=-3))
        self.assertTrue(np.all(theta_lb>=0))
        self.assertTrue(np.all(theta_ub<=1))


if __name__ == '__main__':
    unittest.main()
