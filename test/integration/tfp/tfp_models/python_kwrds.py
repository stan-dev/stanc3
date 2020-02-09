Info: Found int division at 'stan_models/python_kwrds.stan', line 13, column 12 to column 18:
  lambda / 3
Positive values rounded down, negative values rounded up or down in platform-dependent way.

import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

def yield_(b):
  return b * tf__.cast(3, tf__.float64)
   
def func(await_):
  return await_ + tf__.cast(1, tf__.float64)
   
class python_kwrds_model(tfd__.Distribution):

  def __init__(self, lambda_):
    self.lambda_ = lambda_
     
  
  def log_prob_one_chain(self, params):
    target = 0
    lambda_ = self.lambda_
    finally_ = tf__.cast(params[0], tf__.float64)
    assert_ = finally_ + tf__.cast(2, tf__.float64)
    target += tf__.reduce_sum(tfd__.Normal(yield_(assert_),
                                           tf__.cast(1, tf__.float64)).log_prob(d))
    target += tf__.reduce_sum(tfd__.Binomial(tf__.cast(10, tf__.float64),
                                             func(finally_)).log_prob(lambda_))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    lambda_ = self.lambda_
    return [(nchains__, )]
     
  def parameter_bijectors(self):
    lambda_ = self.lambda_
    return [tfb__.Identity()]
     
  def parameter_names(self):
    return ["finally_"]
     
model = python_kwrds_model