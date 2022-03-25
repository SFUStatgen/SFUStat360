# Example based on https://scikit-learn.org/stable/tutorial/statistical_inference/supervised_learning.html#linear-model-from-regression-to-sparsity
from sklearn import datasets
diabetes_X, diabetes_y = datasets.load_diabetes(return_X_y=True)
from sklearn import linear_model
regr = linear_model.LinearRegression()
regr.fit(diabetes_X, diabetes_y)
# The mean square error
MSE = np.mean((regr.predict(diabetes_X) - diabetes_y)**2)
