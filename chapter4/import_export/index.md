
# অধ্যায় ৪: ডেটা ইমপোর্ট এবং এক্সপোর্ট
## Data Import and Export in R
আগের অধ্যায়গুলোতে আমরা R-এর মৌলিক বিষয়গুলো দেখেছি। কিন্তু বাস্তবে যখন আপনি কাজ করবেন তখন সব কাজের শুরু হয় ডেটা import আর  export এর মাধ্যমে। আমি আপনাদেরকে কিভাবে R এর ভেতরে ডেটা তৈরি করা যেমন ধরেন dataframe বানানো, matrix বানানো এগুলা দেখিয়েছি। কিন্তু কাজের সময় আমরা সাধারণত R-এর ভেতরে ডেটা তৈরি করি না। বেশিরভাগ সময় ডেটা আসে কোনো CSV file, Excel sheet এসব এ। আবার কখনও কখনও sequencing output, clinical metadata table এর মাধ্যমে অথবা laboratory measurement file থেকে। তাই প্রথম কাজই হলো সেই data যা আপনি কম্পিউটার এ রেখেছেন সেটা R-এ নিয়ে আসা। আমরা R এর ভাষায় একে R এ ডেটা load/import করা বলি। 

এরপর analysis শেষ হলে আপনি যে ডেটা process করেছেন বা যে ডেটা এর উপর কাজ করেছেন সেটা save করতে হয়। এর বিভিন্ন কারণ থাকতে পারে। যেমন ধরুন আপনার হয়ত পরে আবার ডেটা নিয়ে কাজ করতে হতে পারে, অথবা অন্য কাউকে আপনার ডেটা share করতে হতে পারে। এসব ক্ষেত্রে আপনার R এ process করা ডেটাকে save করতে হয়ে। আমরা R এর ভাষায় ডেটা export করা বলি। এই অধ্যায়ে আমার মূল ইচ্ছা হচ্ছে csv অথবা excel file কিভাবে import করতে হয় সেটা দেখানো এবং ডেটা কিভাবে export করতে হয় সেটাও দেখানো। 
CSV এবং Excel ফাইল থেকে ডেটা ইমপোর্ট করা

# আপডেট পাওয়ার জন্য নিবন্ধন করুন (Register for Updates)

আপনি যদি এই ব্লগের নিয়মিত আপডেট পেতে চান, তাহলে নিচের ফর্মটি পূরণ করুন। আমি নতুন কোনো কন্টেন্ট যোগ করার সাথে সাথেই আপনাকে ইমেইলের মাধ্যমে জানিয়ে দেব।

# [**ফর্ম পূরণ করতে এখানে ক্লিক করুন**](https://forms.gle/6qyRGiE7WSpLJ9SA9)

## Importing Data from CSV and Excel Files
## CSV File কী?
CSV file বলতে বোঝায় Comma-Separated Values। এটি একটি data file format। এখানে প্রতিটি column comma দিয়ে আলাদা করা থাকে।
উদাহরণ:
```r
sample_id,condition,b_cells,t_cells
S1,Healthy,500,1000
S2,Disease,300,850
```
এটি দেখতে খুব সাধারণ text file-এর মতো। কিন্তু spreadsheet software যেমন Excel-এ খুললে table-এর মতো দেখা যায়। যেখানে প্রথম কলাম sample_id, দ্বিতীয় কলাম condition, তৃতীয় কলাম  b_cells আর চতুর্থ কলাম t_cells। 
আপনারা যদি অন্যান্য জায়গা থেকে import বা export সম্পর্কে পড়েন তাহলে দেখবেন যে অনেক রকম file system আছে এবং সেগুলো R এ import করার কোড ভিন্ন। আমি সচরাচর কাজের সুবিধার্থে CSV file ব্যবহার করি। এটি lightweight, সহজে share করা যায় এবং প্রায় সব software-ই এটি support করে এবং কোড এ ঝামেলা কম মনে হয় আমার। যদি এই কোথাটি সম্পূর্ণ আমার মত। অন্যরা অন্য কোন format কে ভাল বলতে পারে।যেহেতু ব্লগ আমি লিখছি, আমি CSV সম্পর্কেই বলবো। 
## Working Directory কী?
R কোনো file import করার আগে জানতে চায় file কোথায় আছে। আপনাকে R কে ওই file এর location জানাতে হবে। যেমন ধরুন আমি এই অধ্যায়ে immune_data.csv নামে একটি file ব্যবহার করবো। এটি আছে আমার computer এ Downloads এর various person project এর blog folder এর chapter ৪ এর মধ্যে। নিচে আমি ওই location কে এভাবে লিখতে পারি। 
```r
/Users/mohammads/Downloads/Various person project/Blogging/chapter 4/immune_data.csv
```
বর্তমানে R কোন folder-এ কাজ করছে সেটি দেখতে:
```r
getwd()
```
Output:
```r
[1] "/Users/mohammads"

```
এটি হলো বর্তমান working directory। আপনি working directory পরিবর্তন করতে পারেন। এটা অনেকটা এরকম যে আপনি যেই folder এ কাজ করবেন সেই folder এ R কে নিয়ে যাওয়া। এখত্রে ব্যবহার করতে হয় setwd()। 
```r
setwd("/Users/mohammads/Downloads/Various person project/Blogging/chapter 4/")
```
এখন R এই folder-এর মধ্যে file খুঁজবে।
## CSV File Import করা
ধরুন, আমাদের একটি file আছে যার নাম  immune_data.csv
এবং file-টির ভেতরে আছে:
sample_id	condition	b_cells	t_cells
S1	Healthy	500	1000
S2	Healthy	620	1100
S3	Disease	300	900
এখন আমরা এটি R-এ import করব।
### Base R দিয়ে CSV Import
```r
immune_data <- read.csv("immune_data.csv")
immune_data

```
এখন data দেখতে:
Output:
```r
  sample_id condition b_cells t_cells
1        S1   Healthy     500    1000
2        S2   Healthy     620    1100
3        S3   Disease     300     900
```
এখানে:

•	read.csv() → CSV file পড়ার function 

•	"immune_data.csv" → file-এর নাম 

প্রথম কয়েকটি row দেখা

Dataset বড় হলে পুরো table print করা ভালো idea না। তাই আমরা প্রথম কয়েকটি row দেখি:
```r
head(immune_data)

```
শেষ কয়েকটি row দেখতে:
```r
tail(immune_data)
```

## Data Structure দেখা
Import করার পরে data ঠিকভাবে এসেছে কি না, সেটা দেখা গুরুত্বপূর্ণ।
```r
str(immune_data)
```
Output:
```r
'data.frame':	3 obs. of  4 variables:
 $ sample_id: chr  "S1" "S2" "S3"
 $ condition: chr  "Healthy" "Healthy" "Disease"
 $ b_cells  : int  500 620 300
 $ t_cells  : int  1000 1100 900
```
এখানে দেখা যাচ্ছে:
•	sample_id হলো character 
•	condition হলো character 
•	b_cells integer 
•	t_cells integer 
এটি গুরুত্বপূর্ণ কারণ অনেক সময় number accidentally character হিসেবে import হয়।
## কত row এবং column আছে?
```r
dim(immune_data)
```
Output:
```r
[1] 3 4
```
মানে হচ্ছে ৩টি  row এবং ৪ টি column ।
শুধু row সংখ্যা আলাদাভাবে বের করতে চাইলে :
```r
nrow(immune_data)
```
শুধু column সংখ্যা বের করতে চাইলে :
```r
ncol(immune_data)
```
## Column নাম দেখা
```r
colnames(immune_data)
```
Output:
```r
[1] "sample_id" "condition" "b_cells"   "t_cells"  
```
## File path দিয়ে import করা
যদি file working directory-তে না থাকে। এটা অনেকসময় হয় যে আপনার file হয়ত অন্য কোন folder এ আছে। সেক্ষেত্রে আপনি file location ব্যবহার করে directly import/load করতে পারবেন। এখানে বলে রাখা ভাল আমি macOS ব্যবহার করছি। আপনি যদি windows ব্যবহার করেন তাহলে লেখার নিয়ম একটু ভিন্ন হবে।
```r
immune_data <- read.csv("/Users/mohammads/Downloads/Various person project/Blogging/chapter 4/immune_data.csv")
```
## Excel File Import করা
অনেক biological data Excel file হিসেবে আসে।
যেমন:

•	clinical metadata 

•	patient information 

•	ELISA result 

•	flow cytometry summary 

•	cell count table 

R-এর base package সরাসরি .xlsx file read করতে পারে না। এজন্য package লাগে।
সবচেয়ে জনপ্রিয় packageগুলোর একটি হলো readxl।
### readxl install করা
```r
install.packages("readxl")
```
তারপর load করি:
```r
library(readxl)
```

### Excel File Import করা
ধরুন file-এর নাম: immune_data.xlsx
এখন import:
```r
immune_data <- read_excel("immune_data.xlsx")
```

## CSV বনাম Excel
একটি গুরুত্বপূর্ণ বিষয়। বাস্তবে bioinformatics analysis-এ অনেকেই CSV prefer করেন। একটা সুবিধা হল আপনি যদি excel ফাইল পান সেঁতা খুব সহজেই অন্য format এ পরিবর্তন করা যায়। 

Import করার পরে সাধারণ ভুলগুলো
### ১. Number character হয়ে যাওয়া
উদাহরণ:
```r
"500"
```
এটি number না। এটি text।
Check করতে:
str(immune_data)। এর মাধ্যমে আপনি বুঝতে পারবেন কোন কলামটি numeric আর কোনটি character ডেটা। যদি এক format এ থাকে সেটাকে অন্য format এ পরিবর্তন করা যায়। as.numeric() অথবা as.character() এরকম ফাংশন ব্যবহার এর মাধ্যমে। 

### ২. Missing value সমস্যা
অনেক সময় missing value আসে:
```r
NA
```
উদাহরণ:
```r
sample_id	b_cells
S1	500
S2	NA
```	
Average বের করতে:
```r
mean(immune_data$b_cells)
```
এতে result NA আসতে পারে।
তাই:
```r
mean(immune_data$b_cells, na.rm = TRUE)
```
na.rm = TRUE মানে missing value ignore করো। এক্ষেত্রে একটা বিষয় মাথায় রাখা খুব জরুরি। এই NA বলতে এটা বোঝায় না যে এখানে 0 আছে। আপনি বুঝবেন যে এটা ফাকা। এবং সেঁতা বিবেচনা করে আপনি কোড লিখবেন। 


## প্রসেসড ডেটা এক্সপোর্ট করা
## Exporting Processed Data
Analysis করার পরে আমরা processed data save করতে চাই।
ধরুন আমরা B/T ratio calculate করেছি।
```r
immune_data$b_t_ratio <- immune_data$b_cells / immune_data$t_cells
```
এখন আমরা updated dataset save করতে চাই।
CSV File Export করা
Base R দিয়ে:
```r
write.csv(immune_data, "processed_immune_data.csv")
```
এখন working directory-তে নতুন file তৈরি হবে।

### Row Name সমস্যা
অনেক সময় export করার পরে অতিরিক্ত number column দেখা যায়। এটি avoid করতে
```r
write.csv(immune_data,
          "processed_immune_data.csv",
          row.names = FALSE)
```
এটি খুব important।
## বাস্তব Bioinformatics Perspective
বাস্তবে sequencing analysis-এ আমরা প্রায় সবসময় file import/export করি।
যেমন:

•	Cell Ranger output matrix 

•	metadata table 

•	differential expression result 

•	clonotype annotation 

•	spatial coordinate table 

•	pathway enrichment result 

একটি analysis-এর বড় অংশই আসলে data management। অনেক নতুন learner মনে করেন bioinformatics মানেই machine learning বা complex statistics। কিন্তু বাস্তবে file handling খুব গুরুত্বপূর্ণ skill।

কারণ:

ভুল file import → ভুল analysis

ভুল metadata → ভুল biological interpretation

Wrong sample annotation → পুরো project-এর সমস্যা 

সুতরাং data handling কোনো “ছোট skill” না। এটি analysis-এর foundation।


## পরিশেষে নিজের কিছু মত। 
ডেটা import এবং export করা হয়তো খুব exciting topic মনে নাও হতে পারে। এখানে machine learning নেই, deep learning নেই, fancy plot নেই। কিন্তু বাস্তবে প্রায় সব analysis এই জায়গা থেকেই শুরু হয়। একটি analysis ঠিকভাবে করতে হলে প্রথমে data ঠিকভাবে load করতে হবে। তারপর processed result cleanভাবে save করতে হবে।
তাই data handling skill যত ভালো হবে, আপনার analysis তত reliable হবে।
শুরুতে এটি একটু boring লাগতে পারে। কিন্তু পরে বুঝবেন, clean data management আসলে ভালো science করার একটি বড় অংশ।

# আপডেট পাওয়ার জন্য নিবন্ধন করুন (Register for Updates)

আপনি যদি এই ব্লগের নিয়মিত আপডেট পেতে চান, তাহলে নিচের ফর্মটি পূরণ করুন। আমি নতুন কোনো কন্টেন্ট যোগ করার সাথে সাথেই আপনাকে ইমেইলের মাধ্যমে জানিয়ে দেব।

# [**ফর্ম পূরণ করতে এখানে ক্লিক করুন**](https://forms.gle/6qyRGiE7WSpLJ9SA9)

