from stan import stan, merge_chains
import unittest
import numpy as np
import json

def significant_digit(x):
    return float("{:.1}".format(x))

def significant_mean(x):
    return significant_digit(np.mean(x))


class TestModels(unittest.TestCase):

    # def test_airt2pl(self):
    #     from irt2pl import model
    #     from irt2pl_data import data
    #     for param in model.parameter_names():
    #         if param in data:
    #             del data[param]
    #     target_dist = model(**data)
    #     mcmc_trace, _ = stan(target_dist)

    #     print([merge_chains(x) for x in mcmc_trace])

        # real<lower=0> sigma_theta;
        # vector[J] theta;
        #
        # real<lower=0> sigma_a;
        # vector<lower=0>[I] a;
        #
        # real mu_b;
        # real<lower=0> sigma_b;
        # vector[I] b;

    def test_eight_schools(self):
        from eight_schools import eight_schools_ncp_model
        import eight_schools_data
        target_dist = eight_schools_ncp_model(**eight_schools_data.data)
        mcmc_trace, _ = stan(target_dist)
        mu, tau, theta_tilde = [merge_chains(x) for x in mcmc_trace]

        self.assertAlmostEqual(significant_mean(mu), 4, delta=2)
        self.assertAlmostEqual(significant_mean(tau), 3, delta=2)
        self.assertAlmostEqual(significant_mean(theta_tilde), 0.08,
                               delta=0.1)

if __name__ == '__main__':
    unittest.main()
