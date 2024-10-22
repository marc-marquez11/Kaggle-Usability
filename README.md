Kaggle is a website that features data science courses, competitions, and datasets. As of April 20th, 2024, there are 317,983 datasets. Said datasets all have a "Usability Rating" (rated from 0-10), which was created to allow users to tell which are well-updated, well-documented, and have well-explained variables. While the components of the formula behind Usability are known, they are vague (e.g. what defines "completeness") and the formula itself is unknown. Thus, the purpose of this project was to take common aspects of a Kaggle dataset (upvotes, types of medals, file size, etc.) and determine if they can accurately predict its Usability. <br>
<br>

The xlsx file shows the majority of the initial cleaning process of the data, which consists of datasets taken from Kaggle itself (source found [here](https://www.kaggle.com/datasets/rajugc/kaggle-dataset)), as well as some initial exploratory analysis. <br>
<br>

The csv file contains the final, cleaned dataset. <br>
<br>

The R file contains all of the code used for the project, including the remaining EDA, modelling, and testing.<br>
<br>

The usability pdf file contains more detail regarding the process of the project, and is written in the style of an academic paper, containing an abstract, introduction, methods, results, and conclusion. <br>
<br>

The presentation pdf file is a concise summary of the usability pdf in the form of a Powerpoint. <br>
<br>

In summary, the final model selected was a boosted regression tree which yielded a mean test error of 5.1983, yielding a poor result for predicting Usability. This suggests that either the data is flawed (may need more data or different predictors) or that Usability itself is flawed, and not a perfect way to determine a dataset's legitimacy.





