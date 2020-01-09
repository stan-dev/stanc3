from tfp_models.eight_schools import eight_schools_ncp_model
from models_data import eight_schools_data, test_disc_binary_data, test_disc_bounded_data, test_disc_unbounded_data, normal_lub_data

from tfp_models.normal_lub import normal_lub_model

from tfp_models.test_cont_01 import test_cont_01_model
from tfp_models.test_cont_positive import test_cont_positive_model
from tfp_models.test_cont_unbounded import test_cont_unbounded_model
from tfp_models.test_cont_lower_bounded import test_cont_lower_bounded_model
from tfp_models.test_disc_binary import test_disc_binary_model
from tfp_models.test_disc_bounded import test_disc_bounded_model
from tfp_models.test_disc_unbounded import test_disc_unbounded_model
from tfp_models.test_circular import test_circular_model

from stan import stan, merge_chains
import unittest
import numpy as np
import tensorflow as tf
import tensorflow_probability as tfp
from os.path import dirname, join
tfb = tfp.bijectors

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
        log_det_corrections = [bijector.inverse_log_det_jacobian(param,0).numpy() for bijector, param in zip(bijectors, stan_samples.T) if not isinstance(bijector, tfb.Identity)]

        if len(log_det_corrections) > 0:
            log_det_corrections = np.stack(log_det_corrections).sum(0)
        else:
            #all bijectors are tfb.Identity  
            log_det_corrections = 0
        self.assertTrue(np.allclose(tfp_lp-log_det_corrections, stan_lp))

    def test_cont_unbounded(self):
        target_dist = test_cont_unbounded_model()
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_cont_unbounded.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_cont_lower_bounded(self):
        target_dist = test_cont_lower_bounded_model()
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_cont_lower_bounded.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_cont_positive(self):
        target_dist = test_cont_positive_model()
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_cont_positive.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_cont_01(self):
        target_dist = test_cont_01_model()
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_cont_01.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_disc_binary(self):
        target_dist = test_disc_binary_model(**test_disc_binary_data.data)
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_disc_binary.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_disc_bounded(self):
        target_dist = test_disc_bounded_model(**test_disc_bounded_data.data)
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_disc_bounded.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_disc_unbounded(self):
        target_dist = test_disc_unbounded_model(**test_disc_unbounded_data.data)
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_disc_unbounded.txt"))
        self._compare_lp(target_dist, stan_fit)

    def test_circular(self):
        target_dist = test_circular_model()
        stan_fit = np.loadtxt(join(dirname(__file__), "stan_samples/samples_circular.txt"))
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
