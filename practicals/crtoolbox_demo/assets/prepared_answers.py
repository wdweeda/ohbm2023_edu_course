
# ----------------------------------------------------------------------------------------------
# Test your understanding: The images we have looked at so far can all be found in the 
# `./demo/data/example1` folder. Another set of CRs can be found in `./demo/data/example2` 
# folder under similar filenames. Using the functions described above, have a look at these 
# files. If these were your analysis results, which CRs do you think would be preferable to 
# observe? And why?
# ----------------------------------------------------------------------------------------------

# Load in pre-computed confidence regions.
Upper_CR_fname = os.path.join(os.getcwd(),'demo','data','example2','Upper_CR_Example2.nii.gz')
Lower_CR_fname = os.path.join(os.getcwd(),'demo','data','example2','Lower_CR_Example2.nii.gz')

# Load in a pre-computed point estimate of Ac.
estimated_Ac_fname = os.path.join(os.getcwd(),'demo','data','example2','Estimated_Ac_Example2.nii.gz')

# Load in a pre-computed mask.
mask_fname = os.path.join(os.getcwd(),'demo','data','mask.nii.gz')

# Display confidence regions in interactive slice plot.
display_crs(estimated_Ac_fname, Upper_CR_fname, Lower_CR_fname, mask_fname, mode='Sagittal')







# ----------------------------------------------------------------------------------------------
# Choose your noise and signal
# ----------------------------------------------------------------------------------------------
my_noise = Noise(var=8)
my_signal = CircleSignal(r=20, mag=3)







# ----------------------------------------------------------------------------------------------
# Let's perform a regression to get the images we need. You can use the same `regression`
# function as in the 2D example to perform this step.
# ----------------------------------------------------------------------------------------------
betahat_files, sigma_file, resid_files = regression(y_files, X, out_dir)






# ----------------------------------------------------------------------------------------------
# Have a look at these CRs by writing some code in the box below.
# ----------------------------------------------------------------------------------------------
display_crs(estimated_ac_file, upper_cr_file, lower_cr_file, mask_file)




# ----------------------------------------------------------------------------------------------
# **The Challenge**
#
# Your challenge is to fit a regression model to this data and generate confidence regions. Try
# the following model, generate CRs, and see if you can determine which regions of activation
# you have high confidence in.
#
# $$\text{COPE}(s) = \beta_0 + \text{Sex} \beta_1 + \text{Age} \beta_2 + \text{PMAT24\_A\_CR} 
# \beta_3 + \text{PSQI\_SCORE} \beta_4$$
#
# If you have any questions, or are unsure how to do this, please feel free to ask.
# ----------------------------------------------------------------------------------------------

# First, insert the new column of ones at position 0
if 'Intercept' not in covariates.columns:
    covariates.insert(0, 'Intercept', 1)

# Convert the DataFrame to a numpy array
X = covariates.drop('Subject', axis=1).values

# Output directory (feel free to change this to your desired output directory)
out_dir = os.path.join(real_data_dir, 'results')
if not os.path.exists(out_dir):
    os.mkdir(out_dir)

# Fit the regression model
betahat_files, sigma_file, resid_files = regression(bold_files, X, out_dir, chunk_size=5)

# We'll add a mask as a background to help visualise the data
mask_file = os.path.join(os.getcwd(),'data','mask.nii.gz')

# Threshold c
c = 20

# Decide a confidence level alpha
alpha = 0.05

# Compute confidence regions
lower_cr_file, upper_cr_file, estimated_ac_file, quantile_estimate = generate_CRs(betahat_files[0], sigma_file, resid_files, out_dir, c, 1-alpha, n_boot=2000)

# Display the crs
display_crs(estimated_ac_file, upper_cr_file, lower_cr_file, mask)

# Remove files
remove_files(bold_files, resid_files, betahat_files, sigma_file, lower_cr_file, upper_cr_file, estimated_ac_file)