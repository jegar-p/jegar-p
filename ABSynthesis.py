""" ABSynthesis API helper functions for Python users

A Python wrapper of helper functions to use the ABSynthesis API.
"""

import requests
import os
import random
import numpy.random as npr


def test_key():
        if 'rapidapi_key' in os.environ:
            return os.environ['rapidapi_key']
        else:
    # If not found anywhere, return None or handle the case appropriately
            return "No key called rapidapi_key found in the Python environment, and no api_key parameter has been supplied. If you don't have a key, you can download one from https://rapidapi.com/api/absynthesis. If you have one downloaded already, please ensure it is stored correctly in your environment with the name 'rapidapi_key', or that you have referenced it correctly in the function call. Run  os.environ['rapidapi_key'] to test whether the key is in your environment, and you can run register_rapidapi_key() to add it to your environment for the session."

def ABSynthesis_summary(successes_base, visitors_base, successes_variant, visitors_variant, api_key = None):
    """
    Combine AB test results and get summary statistics.

    ABSynthesis_summary returns summary statistics combining the results of all online controlled experiments.
    Returns summary statistics of a meta-analysis of online controlled experiments.


    Parameters
    -----------
    successes_base : List of counts of responses in the base condition, 1 for each study

    visitors_base :  List of counts of visitors exposed to the base condition, 1 for each study

    successes_variant : List of counts of responses in the base condition, 1 for each study

    visitors_variant : List of counts of visitors exposed to the base condition, 1 for each study

    api_key : A RapidAPI API key stored in a separate file. If not provided, the function will check the environment for 'rapidapi_key'.
    """

    if api_key is None:
        rapidapi_key = test_key()
    else:
        rapidapi_key = api_key
    paramset = {'successes_base':successes_base, 'visitors_base':visitors_base, 'successes_variant':successes_variant, 'visitors_variant':visitors_variant}
    Headers = {'x-rapidapi-host':'absynthesis.p.rapidapi.com',
    'x-rapidapi-key':rapidapi_key}
    return requests.get(url = "https://absynthesis.p.rapidapi.com/summary_results", params = paramset,
    headers=Headers).json()

def ABSynthesis_advanced(successes_base, visitors_base, successes_variant, visitors_variant, api_key = None):
    """
    Combine AB test results and get advanced statistics for each study in the set.

    ABSynthesis_advanced returns results of each study in the set accounting for the overall effect of all other studies.

    Parameters
    -----------
    successes_base : List of counts of responses in the base condition, 1 for each study

    visitors_base :  List of counts of visitors exposed to the base condition, 1 for each study

    successes_variant : List of counts of responses in the base condition, 1 for each study

    visitors_variant : List of counts of visitors exposed to the base condition, 1 for each study

    api_key : A RapidAPI API key stored in a separate file. If not provided, the function will check the environment for 'rapidapi_key'.
    """

    if api_key is None:
        rapidapi_key = test_key()
    else:
        rapidapi_key = api_key
    paramset = {'successes_base':successes_base, 'visitors_base':visitors_base, 'successes_variant':successes_variant, 'visitors_variant':visitors_variant}
    Headers = {'x-rapidapi-host':'absynthesis.p.rapidapi.com',
    'x-rapidapi-key':rapidapi_key}

    return requests.get(url = "https://absynthesis.p.rapidapi.com/individual_experiment_results", params = paramset, headers=Headers).json()

def register_rapidapi_key(key):
    """
    Add a RapidAPI key to your session environment.

    register_rapidapi_key is a helper function that adds your RapidAPI key to your R environment. You will need to provide an API key which can be downloaded from rapidapi.com/hub.

    Parameters
    -----------

    key : A string representing an API key downloaded from RapidAPI.com/hub

    """
    os.environ['rapidapi_key'] = key
    print("RapidAPI key updated in the environment")


def initialize_synthetic_AB_data(outcomes = ['converted', 'did not convert'], experiment_visitors = [5000,10000], baseline_probability = 0.01, variant_probability = 0.02 ):
    """
    Generate a synthetic experiment result based on desired conversion probabilities.

    initialize_synthetic_AB_data simulates an AB test. The user can define the underlying conversion probabilities in each condition of an experiment.

    Returns a set of numbers that represent what would be seen at the end of one AB test with the underlying input parameters.

    Parameters
    --------------

    outcomes A list of possible experiment outcomes

    experiment_visitors The minimum and maximum number of visitors that should be simulated in the experiment

    baseline_probability Proportion of visitors that perform the desired action in the base version

    variant_probability Proportion of visitors that perform the desired action in the variant version

    """

    AB_x = outcomes
    size_min = min(experiment_visitors)
    size_max = max(experiment_visitors)
    AB_size = float(random.sample(range(size_min,size_max), 1)[0])

    base_size = AB_size / 2 + random.sample(range(0, int(AB_size * 0.025)), 1)[0]
    variant_size = AB_size - base_size

    base_prob = [baseline_probability, 1-baseline_probability]
    variant_prob = [variant_probability,1-variant_probability]

    base_visitors = list(npr.choice(outcomes, int(base_size), p = base_prob))
    variant_visitors = list(npr.choice(outcomes, int(variant_size), p = variant_prob))

    my_experiment_summary = {"successes_base" : base_visitors.count(outcomes[0]),"visitors_base" : base_visitors.count(outcomes[1]),"successes_variant" : variant_visitors.count(outcomes[0]),"visitors_variant" : variant_visitors.count(outcomes[1])}
    return my_experiment_summary

def initialize_synthetic_AB_corpus(experiments, experiment_visitors = [5000,10000], baseline_probability = 0.01, variant_probability = 0.02 ):
    """
    Generate a synthetic experiment result based on desired conversion probabilities.

    initialize_synthetic_AB_data simulates an AB test. The user can define the underlying conversion probabilities in each condition of an experiment.

    Returns a set of numbers that represent what would be seen at the end of one AB test with the underlying input parameters.

    Parameters
    --------------

    experiments The number of experiments to be simulated

    experiment_visitors The minimum and maximum number of visitors that could be simulated in each experiment as a list

    baseline_probability Proportion of visitors that perform the desired action in the base version

    variant_probability Proportion of visitors that perform the desired action in the variant version

    """
    successes_base_list = []
    visitors_base_list = []
    successes_variant_list = []
    visitors_variant_list = []

    for i in range(experiments):
        x = initialize_synthetic_AB_data()
        successes_base_list.append(x['successes_base'])
        visitors_base_list.append(x['visitors_base'])
        successes_variant_list.append(x['successes_variant'])
        visitors_variant_list.append(x['visitors_variant'])

    return{"successes_base" : successes_base_list, "visitors_base" : visitors_base_list, "successes_variant" : successes_variant_list,"visitors_variant" : visitors_variant_list}


if __name__ == "__main__":
    ABSynthesis_advanced([10,12,15], [30,30,30], [12, 15, 20], [30, 30, 30])
