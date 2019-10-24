from eight_schools import eight_schools_ncp_model
import eight_schools_data
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

if __name__ == '__main__':
    unittest.main()
