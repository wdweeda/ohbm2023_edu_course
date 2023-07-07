
# ----------------------------------------------------------------------------------------------
# As a sanity check, we can assess the coverage of our confidence regions by using the `check_violations` function.
# ----------------------------------------------------------------------------------------------
# Set the number of repetitions (to begin this is set to 1000)
nReps = 1000

# Get the number of observations
n = len(data_files)

# Get alpha
alpha = 0.05

# Loop through the repetitions
for j in np.arange(nReps):

    # Generate some new noise
    my_noise.generate()

    # Generate some new data
    data_files, mu_file = generate_data_2D(my_signal, my_noise, out_dir=out_dir)

    # Fit the regression model
    muhat_file, sigma_file, resid_files = regression(data_files, X, out_dir)

    # Generate Confidence Regions
    lower_cr_file, upper_cr_file, estimated_ac_file, quantile_estimate = generate_CRs(muhat_file, sigma_file, resid_files, out_dir, c, 1-alpha, n_boot=5000)

    # Get the results
    results = check_violations(upper_cr_file, lower_cr_file, muhat_file, sigma_file, mu_file, n, c, quantile_estimate)

    # Average the results
    if j == 0:
        concat_results = np.array(results[0])
    else:
        # Concatenate the results
        concat_results = np.concatenate((concat_results.reshape(1,np.prod(concat_results.shape)),
                                        results[0].reshape(1,np.prod(results[0].shape))),axis=1)

        # Avergae the results
        avg_results = np.mean(concat_results,axis=1)

        print('current avg: ', avg_results[0])


# ----------------------------------------------------------------------------------------------
# > **Test your understanding:** Try running the above with a few different signals and noise
# settings. Which factors (e.g. noise magnitude, sample size, smoothness) cause the observed
# coverage to differ from $1-\alpha$? Why do you think this is?
# ----------------------------------------------------------------------------------------------
