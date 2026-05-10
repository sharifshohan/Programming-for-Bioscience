
# Data Handling with dplyr 
## dplyr এবং এর প্রধান ফাংশন
আগের অধ্যায়গুলোতে আমরা R-এর মৌলিক বিষয়গুলো দেখেছি। ভেরিয়েবল কী, ভেক্টর কীভাবে কাজ করে, if-else, লুপ, ফাংশন, এসব নিয়ে আলোচনা করেছি। এখন আমরা একটু আসল ডেটার দিকে একটু নজর দেওয়ার চেষ্টা করি। 
বায়োসায়েন্সে ডেটা সাধারণত একক কোনো সংখ্যা না। এটা বলতে আমি বোঝানোর চেষ্টা করছি যে একটা value থাকে ব্যাপার টা এমন না। বেশিরভাগ সময় ডেটাগুলো টেবিলে সাজানো থাকে। আমরা আবার immunology এর দিক থেকে চিন্তা করি। যেমন, কোনো একটা গবেষণা হল এবং বিভিন্ন sample থেকে B cell, T cell, CD4 T cell, CD8 T cell, plasma cell বা memory B cell এর সংখ্যা মাপা হল। এখন আমরা কিছু প্রশ্ন করতে চাইঃ 

কোন sample-এ B cell বেশি?

কোন condition-এ T cell কম?

Healthy আর Disease sample-এর মধ্যে পার্থক্য আছে কি?

প্রতিটি group-এর average cell count কত?

এই ধরনের কাজের জন্য R-এ অনেক পদ্ধতি আছে। আমরা চাইলে base R দিয়েই করতে পারি। কিন্তু ডেটা বড় হলে base R কোড অনেক সময় দেখতে কঠিন হয়ে যায়। এখানেই dplyr খুব কাজে আসে।
dplyr হলো R-এর একটি package, যা data frame বা table নিয়ে কাজ করার জন্য খুব জনপ্রিয়। এটি tidyverse নামের বড় package collection-এর অংশ।
আমাদের কাজের এই অংশে আমরা এই dplyr এর আদ্যোপান্ত সম্পর্কে জানব। আমি আরেকটি জিনিস একইসাথে করবো। সেটা হচ্ছে base R এর ফাংশন দিয়েও একই কাজ করা যায়। আমি দুটোই পাশাপাশি দেখাব। 
প্রথম কাজ হচ্ছে  package load করা। 
```r
library(dplyr)
```
যদি install করা না থাকে, তাহলে আগে install করতে হবে। সেক্ষেত্রে install করার জন্য নিচের কোড ব্যবহার করতে পারেন। 
```r
install.packages("dplyr")
library(dplyr)
```
আমি যেহেতু বলেছি যে পুরা ব্যাখ্যা আমি immunology দিয়ে করার চেষ্টা করবো। এজন্য শুরুতে আমাদের একটা ডেটা বানাতে হবে। এই জিনিসটি দুইভাবে করা যায়। আপনার যদি কোথাও ডেটা থাকে ,ধরুন excel ফাইল এ, সেক্ষেত্রে আপনি ওই ফাইলটি load করবেন অথবা আপনি data.frame ব্যাবহার করে একটা ডেটা বানাবেন। আমরা ধরুন একটা কাল্পনিক ডেটা বানাই। এই ডেটাকে ব্যবহার করে আমরা dplyr  package বোঝার চেষ্টা করবো। 
```r
immune_data <- data.frame(
  sample_id = c("S1", "S2", "S3", "S4", "S5", "S6"),
  condition = c("Healthy", "Healthy", "Disease", "Disease", "Healthy", "Disease"),
  b_cells = c(500, 620, 300, 280, 700, 250),
  t_cells = c(1000, 1100, 900, 850, 1200, 800),
  cd4_t_cells = c(600, 650, 500, 480, 700, 450),
  cd8_t_cells = c(400, 450, 400, 370, 500, 350)
)

immune_data
```
Output:
```r
> immune_data
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
1        S1   Healthy     500    1000         600         400
2        S2   Healthy     620    1100         650         450
3        S3   Disease     300     900         500         400
4        S4   Disease     280     850         480         370
5        S5   Healthy     700    1200         700         500
6        S6   Disease     250     800         450         350
```
## ১. select() - নির্দিষ্ট column বেছে নেওয়া
আমাদের dataset-এ ছয়টি column আছে। কিন্তু আপাতত আমরা শুধু sample ID, condition এবং B cell count দেখতে চাই। বাকিগুলো ধরুন এই মুহূর্তে আপনার দরকার নেই। আমরা এই কাজটি dplyr এর select() ফাংশন দিয়ে করতে পারি। 
### dplyr দিয়ে:
```r
immune_data %>%
  select(sample_id, condition, b_cells)
```
Output:
```r
  sample_id condition b_cells
1        S1   Healthy     500
2        S2   Healthy     620
3        S3   Disease     300
4        S4   Disease     280
5        S5   Healthy     700
6        S6   Disease     250
```
এখানে %>% হলো pipe operator। সহজভাবে বললে, এর মানে হলো “এরপর এই কাজটি করো”। 
```r
immune_data %>% select(sample_id, condition, b_cells)
```
এর মানে দাঁড়াচ্ছে এরকম অনেকটা এরকমঃ 
immune_data নাও, তারপর এর মধ্যে থেকে sample_id, condition, আর b_cells column বেছে নাও।

### Base R দিয়ে একই কাজ:
```r
immune_data[, c("sample_id", "condition", "b_cells")]
```
এটাও একই output দেবে। Base R খারাপ না। কিন্তু যখন কাজ বড় হয়, তখন select() পড়তে একটু সহজ লাগে। কারণ কোড দেখেই বোঝা যায় আমরা column select করছি। এটা অনেকটা ইংরেজি তে পড়ার মত বোঝা যায় যে কি হচ্ছে। কিন্তু আপনি base R দিয়ে সম্পূর্ণ কাজ করতে পারবেন। 
 
## ২. filter()-নির্দিষ্ট row বেছে নেওয়া
এবার ধরুন, আমরা শুধু Healthy sample দেখতে চাই।
### dplyr দিয়ে:
```r
immune_data %>%
  filter(condition == "Healthy")
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
1        S1   Healthy     500    1000         600         400
2        S2   Healthy     620    1100         650         450
3        S5   Healthy     700    1200         700         500
```
এখানে condition == "Healthy" মানে যেসব row-তে condition Healthy, শুধু সেগুলো থাকবে ।
### Base R দিয়ে একই কাজ:
```r
immune_data[immune_data$condition == "Healthy", ]
```
এখানে comma-এর আগে row condition, আর comma-এর পরে column condition দেওয়া হয়। আমরা যেহেতু সব column রাখতে চাই, তাই comma-এর পরে কিছু দেইনি।
আরেকটা উদাহরণ দেখি।
ধরুন, আমরা এমন sample চাই যেখানে B cell count 400-এর বেশি।
### dplyr:
```r
immune_data %>%
  filter(b_cells > 400)
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
1        S1   Healthy     500    1000         600         400
2        S2   Healthy     620    1100         650         450
3        S5   Healthy     700    1200         700         500
```
### Base R:
```r
immune_data[immune_data$b_cells > 400, ]
```
এবার যদি আমরা Healthy sample এ B cell count 600-এর বেশি চাই:
### dplyr:
```r
immune_data %>%
  filter(condition == "Healthy", b_cells > 600)
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
1        S2   Healthy     620    1100         650         450
2        S5   Healthy     700    1200         700         500
```
### Base R:
```r
immune_data[immune_data$condition == "Healthy" & immune_data$b_cells > 600, ]
```
এখানে & মানে দুইটি condition-ই সত্য হতে হবে।
 
## ৩. mutate()- নতুন column তৈরি করা
গবেষণায় ডেটা বিশ্লেষণে আমরা প্রায়ই নতুন হিসাব করি। যেমন B cell এবং T cell ratio বের করা। আমরা এক্ষেত্রে mutate() ফাংশন ব্যবহার করে একটা নতুন column এ ratio যোগ করতে পারি। 

B cell to T cell ratio:
```r
b_t_ratio = b_cells / t_cells
```
### dplyr দিয়ে:
```r
immune_data %>%
  mutate(b_t_ratio = b_cells / t_cells)
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells b_t_ratio
1        S1   Healthy     500    1000         600         400 0.5000000
2        S2   Healthy     620    1100         650         450 0.5636364
3        S3   Disease     300     900         500         400 0.3333333
4        S4   Disease     280     850         480         370 0.3294118
5        S5   Healthy     700    1200         700         500 0.5833333
6        S6   Disease     250     800         450         350 0.3125000
```
### Base R দিয়ে একই কাজ:
```r
immune_data$b_t_ratio <- immune_data$b_cells / immune_data$t_cells
immune_data
```
Base R-এ আমরা original data frame-এর মধ্যে নতুন column যোগ করেছি। mutate() দিয়ে চাইলে নতুন object-ও বানানো যায়। অর্থাৎ আপনি চাইলে একটা নতুন data frame তৈরি করতে পারেন। এর কারণ হতে পারে কোন কারণে আপনার আগের data frame এর দরকার পড়ল।
```r
immune_data_ratio <- immune_data %>%
  mutate(b_t_ratio = b_cells / t_cells)
```
আরেকটি উদাহরণ: CD4 এবং CD8 T cell ratio বের করি।
### dplyr:
```r
immune_data %>%
  mutate(cd4_cd8_ratio = cd4_t_cells / cd8_t_cells)

```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells cd4_cd8_ratio
1        S1   Healthy     500    1000         600         400      1.500000
2        S2   Healthy     620    1100         650         450      1.444444
3        S3   Disease     300     900         500         400      1.250000
4        S4   Disease     280     850         480         370      1.297297
5        S5   Healthy     700    1200         700         500      1.400000
6        S6   Disease     250     800         450         350      1.285714

```
### Base R:
```r
immune_data$cd4_cd8_ratio <- immune_data$cd4_t_cells / immune_data$cd8_t_cells
```
এই ধরনের ratio immunology-তে অনেক গুরুত্বপূর্ণ হতে পারে। যেমন কোনো inflammatory condition-এ CD8 T cell বাড়ছে কি না, CD4/CD8 ratio কমছে কি না, এসব প্রশ্ন করা যায়।
## ৪. arrange()-ডেটা সাজানো
ধরুন, আমরা sample গুলো B cell count অনুযায়ী ছোট থেকে বড় সাজাতে চাই।
### dplyr দিয়ে:
```r
immune_data %>%
  arrange(b_cells)
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
1        S6   Disease     250     800         450         350
2        S4   Disease     280     850         480         370
3        S3   Disease     300     900         500         400
4        S1   Healthy     500    1000         600         400
5        S2   Healthy     620    1100         650         450
6        S5   Healthy     700    1200         700         500
```
বড় থেকে ছোট সাজাতে চাইলে:
```r
immune_data %>%
  arrange(desc(b_cells))
```
Output:
```r
  sample_id condition b_cells t_cells cd4_t_cells cd8_t_cells
5        S5   Healthy     700    1200         700         500
2        S2   Healthy     620    1100         650         450
1        S1   Healthy     500    1000         600         400
3        S3   Disease     300     900         500         400
4        S4   Disease     280     850         480         370
6        S6   Disease     250     800         450         350

```

### Base R দিয়ে ছোট থেকে বড়:
```r
immune_data[order(immune_data$b_cells), ]
```
Base R দিয়ে বড় থেকে ছোট:
```r
immune_data[order(immune_data$b_cells, decreasing = TRUE), ]
```
arrange() এখানে বেশি সহজে পড়া যায়। desc() দেখলেই বোঝা যায় descending order-এ সাজানো হচ্ছে।
## ৫. summarise()- সারাংশ বের করা
ধরুন, আমরা সব sample মিলিয়ে average B cell count বের করতে চাই।
### dplyr দিয়ে:
```r
immune_data %>%
  summarise(mean_b_cells = mean(b_cells))
```
Output:
```r
  mean_b_cells
1     441.6667 
```
### Base R দিয়ে:
```r
mean(immune_data$b_cells)
```
এটা খুব সহজ। কিন্তু summarise()-এর সুবিধা হলো আমরা একসাথে অনেক summary বের করতে পারি। ধরুন mean , sum সবকিছু একসাথে বের করে ফেলতে পারি একই কোড এর মধ্যে। 
```r
immune_data %>%
  summarise(
    mean_b_cells = mean(b_cells),
    mean_t_cells = mean(t_cells),
    total_b_cells = sum(b_cells),
    total_t_cells = sum(t_cells)
  )
```
Output:

```r
  mean_b_cells mean_t_cells total_b_cells total_t_cells
1     441.6667          975          2650          5850

```
Base R দিয়ে একই কাজ। এখত্রে আমাদের নতুন একটি data frame তৈরি করতে হবে আলাদাভাবে । 
```r 
data.frame(
  mean_b_cells = mean(immune_data$b_cells),
  mean_t_cells = mean(immune_data$t_cells),
  total_b_cells = sum(immune_data$b_cells),
  total_t_cells = sum(immune_data$t_cells)
)
```
দুটোই ঠিক। তবে summarise() বেশি readable, বিশেষ করে যখন pipeline-এর অংশ হিসেবে ব্যবহার করা হয়।
 
## ৬. group_by() + summarise()- group অনুযায়ী summary বের করা
এটা dplyr-এর সবচেয়ে দরকারি অংশগুলোর একটি। আমার নিজের কাজের ক্ষেত্রে এই ফাংশনটি আমি সবচেয়ে বেশি ব্যবহার করি। কারণ এটার alternative যে base R এর কোড সেটা একটু বড় হয়। 
ধরুন, আমরা Healthy আর Disease condition অনুযায়ী average B cell count জানতে চাই।
### dplyr দিয়ে:
```r
immune_data %>%
  group_by(condition) %>%
  summarise(mean_b_cells = mean(b_cells))
```
Output:
```r
# A tibble: 2 × 2
  condition mean_b_cells
  <chr>            <dbl>
1 Disease           277.
2 Healthy           607.
```
এখানে কী হলো?
প্রথমে data condition অনুযায়ী ভাগ হলো।
তারপর প্রতিটি condition-এর জন্য আলাদা করে mean বের হলো।

### আরেকটি উদাহরণ দেখি। Healthy এবং Disease group অনুযায়ী average B cell, average T cell, আর average CD4/CD8 ratio বের করি।
### dplyr:
```r
immune_data %>%
  mutate(cd4_cd8_ratio = cd4_t_cells / cd8_t_cells) %>%
  group_by(condition) %>%
  summarise(
    mean_b_cells = mean(b_cells),
    mean_t_cells = mean(t_cells),
    mean_cd4_cd8_ratio = mean(cd4_cd8_ratio)
  )
```
Output:
```r
# A tibble: 2 × 4
  condition mean_b_cells mean_t_cells mean_cd4_cd8_ratio
  <chr>            <dbl>        <dbl>              <dbl>
1 Disease           277.          850               1.28
2 Healthy           607.         1100               1.45
```
### Base R দিয়ে:
```r
immune_data$cd4_cd8_ratio <- immune_data$cd4_t_cells / immune_data$cd8_t_cells

aggregate(
  cbind(b_cells, t_cells, cd4_cd8_ratio) ~ condition,
  data = immune_data,
  FUN = mean
)
```
এখানে aggregate() ভালো কাজ করছে। কিন্তু অনেকের কাছে dplyr pipeline বেশি natural লাগে। কারণ ধাপে ধাপে বোঝা যায়:
যেমনঃ ডেটা নাও, নতুন ratio বানাও, condition অনুযায়ী group করো, summary বের করো
ছোট ছোট ধাপ। পরিষ্কার ভাবে বোঝা যায় কি করছি। আমি এই ফাংশনটি সবচেয়ে বেশি ব্যবহার করি আমার কাজে। আর aggeregate() ফাংশন ব্যবহার করা ছাড়া অন্যভাবেও করা যায়। কিন্তু সেক্ষেত্রে আরও কয়েকটি নতুন কোড লিখতে হবে। 
 
## ৭. count()- কোনো group-এ কত sample আছে
ধরুন, আমাদের dataset-এ Healthy আর Disease sample কয়টি করে আছে জানতে চাই।
### dplyr:
```r
immune_data %>%
  count(condition)
```
Output:
```r
  condition n
1   Disease 3
2   Healthy 3
```
### Base R:
```r
table(immune_data$condition)
```
Output:
```r
Disease Healthy 
      3       3
```
### আরেকটা উদাহরণ: আমরা cell count থেকে high এবং low B cell group বানাতে চাই।
```r
immune_data %>%
  mutate(b_cell_group = ifelse(b_cells > 400, "High_B_cell", "Low_B_cell")) %>%
  count(condition, b_cell_group)
```
Output:
```r
  condition b_cell_group n
1   Disease   Low_B_cell 3
2   Healthy  High_B_cell 3

```
## Base R দিয়ে:
```r
immune_data$b_cell_group <- ifelse(
  immune_data$b_cells > 400,
  "High_B_cell",
  "Low_B_cell"
)

table(immune_data$condition, immune_data$b_cell_group)

```
Output:
```r
          High_B_cell Low_B_cell
  Disease           0          3
  Healthy           3          0

```
এই ধরনের কাজ single-cell analysis-এও হয়। যেমন কোনো cluster-এ কত memory B cell আছে, কোন patient group-এ কত exhausted T cell আছে, এসব গণনা করতে হয়। এক্ষেত্রে আমার নিজের কাছে count() ব্যবহার করার থেকে base R এর table() ফাংশনটি বেশি সহজবোধ্য মনে হয়। এজন্য বলতে চাই যে আপনারা যখন ব্যবহার করবেন মাথাত রাখবেন কোনটি আপনার কাছে বেশি বোধগম্য হয়। dplyr package এর ফাংশনই ব্যবহার করতে হবে এমন কোন কোথা নেই। 
## ৮. rename()- column-এর নাম পরিবর্তন করা
অনেক সময় dataset-এ column name পরিষ্কার থাকে না। যেমন b_cells না লিখে কেউ লিখেছে bc, অথবা TcellTotal। তখন নাম পরিবর্তন করা দরকার হয়।
ধরুন, আমরা b_cells column-এর নাম বদলে B_cell_count করতে চাই।
## dplyr:
```r
immune_data %>%
  rename(B_cell_count = b_cells)
```
Output:
```r
  sample_id condition B_cell_count t_cells cd4_t_cells cd8_t_cells cd4_cd8_ratio b_cell_group
1        S1   Healthy          500    1000         600         400      1.500000  High_B_cell
2        S2   Healthy          620    1100         650         450      1.444444  High_B_cell
3        S3   Disease          300     900         500         400      1.250000   Low_B_cell
4        S4   Disease          280     850         480         370      1.297297   Low_B_cell
5        S5   Healthy          700    1200         700         500      1.400000  High_B_cell
6        S6   Disease          250     800         450         350      1.285714   Low_B_cell

```
## Base R:
```r
names(immune_data)[names(immune_data) == "b_cells"] <- "B_cell_count"
```
rename()-এ format হলো:
new_name = old_name
এটা মনে রাখা জরুরি। অনেকেই উল্টো লিখে ফেলেন।
এগুলো ছাড়াও আরও কিছু ফাংশন আছে কেমন slice(), distinct(), pull() ইত্যাদি । আমি কখনও ব্যবহার করি নাই কারণ এগুলোর সবগুলোর alternative base R এর ফাংশন আছে এবং খুবই ভালভাবে বোঝা যায়। আপনারা যদি দেখতে চান তাহলে documentation থেকে দেখে নিতে পারবেন। 

## ৯. %>% pipe operator- ধাপে ধাপে কাজ করা
dplyr-এর আসল শক্তি বোঝা যায় pipe ব্যবহার করলে।
ধরুন, আমরা Disease sample নিতে চাই, তারপর B/T ratio বানাতে চাই, তারপর ratio অনুযায়ী সাজাতে চাই, তারপর শুধু sample_id এবং ratio দেখতে চাই।
### dplyr:
```r
immune_data %>%
  filter(condition == "Disease") %>%
  mutate(b_t_ratio = b_cells / t_cells) %>%
  arrange(desc(b_t_ratio)) %>%
  select(sample_id, condition, b_t_ratio)
```
এটা পড়া যায় এভাবে:
ডেটা নাও, শুধু Disease sample রাখো, B/T ratio বানাও, ratio অনুযায়ী বড় থেকে ছোট সাজাও, শেষে দরকারি column দেখাও। 
### Base R দিয়ে একই কাজ:
```r
disease_data <- immune_data[immune_data$condition == "Disease", ]

disease_data$b_t_ratio <- disease_data$b_cells / disease_data$t_cells

disease_data <- disease_data[order(disease_data$b_t_ratio, decreasing = TRUE), ]

disease_data[, c("sample_id", "condition", "b_t_ratio")]
```
Base R কাজ করছে। অবশ্যই করছে। কিন্তু dplyr pipeline-এ কাজের flow অনেক পরিষ্কারভাবে দেখা যায়।
## ১০. একটি বাস্তবধর্মী ছোট বিশ্লেষণ
এখন আমরা একটি ছোট biological question করি।
## প্রশ্ন:
### Healthy sample-এ কি Disease sample-এর তুলনায় average B/T cell ratio বেশি?
প্রথমে ratio বানাই, তারপর condition অনুযায়ী mean বের করি।
```r
immune_data %>%
  mutate(b_t_ratio = b_cells / t_cells) %>%
  group_by(condition) %>%
  summarise(mean_b_t_ratio = mean(b_t_ratio))
```
Output:
```r
# A tibble: 2 × 2
  condition mean_b_t_ratio
  <chr>              <dbl>
1 Disease            0.325
2 Healthy            0.549
```
এই ছোট উদাহরণে দেখা যাচ্ছে Healthy sample-এ B/T ratio বেশি। অবশ্যই এটা real biological conclusion না। কারণ এই ডেটাসেট কাল্পনিক , মানে আমাদের বানানো।  কিন্তু analysis-এর logic বাস্তবের মতোই।
### Base R দিয়ে:
```r
immune_data$b_t_ratio <- immune_data$b_cells / immune_data$t_cells

aggregate(
  b_t_ratio ~ condition,
  data = immune_data,
  FUN = mean
)
```

## ১১. আরেকটি example: CD8 T cell proportion
ধরুন, আমরা জানতে চাই মোট T cell-এর মধ্যে CD8 T cell-এর proportion কত।
```r
immune_data %>%
  mutate(cd8_fraction = cd8_t_cells / t_cells)
```
### Base R:
```r
immune_data$cd8_fraction <- immune_data$cd8_t_cells / immune_data$t_cells
```
এবার condition অনুযায়ী average CD8 fraction:
```r
immune_data %>%
  mutate(cd8_fraction = cd8_t_cells / t_cells) %>%
  group_by(condition) %>%
  summarise(mean_cd8_fraction = mean(cd8_fraction))
```

Output:
```r
# A tibble: 2 × 2
  condition mean_cd8_fraction
  <chr>                 <dbl>
1 Disease               0.439
2 Healthy               0.409

```
### Base R:
```r

immune_data$cd8_fraction <- immune_data$cd8_t_cells / immune_data$t_cells

aggregate(
  cd8_fraction ~ condition,
  data = immune_data,
  FUN = mean
)
```
এই ধরনের হিসাব immunology-তে খুব common। যেমন tumour sample-এ CD8 T cell infiltration বেশি কি না, disease condition-এ CD4/CD8 balance বদলাচ্ছে কি না, এসব প্রশ্নে আমরা ratio বা fraction ব্যবহার করি।
## dplyr কেন শিখব?
এখন একটা প্রশ্ন আসতে পারে। যেহেতু base R দিয়েও সব করা যাচ্ছে, তাহলে dplyr শিখব কেন?কারণ dplyr কোডকে মানুষের ভাষার কাছাকাছি করে। বিশেষ করে data analysis-এর  ক্ষেত্রে। যেমন: Healthy sample নাও।, B/T ratio বানাও। Average বের করো।
Base R দিয়েও করা যায়। কিন্তু বড় project-এ dplyr pipeline অনেক সময় বেশি পরিষ্কার থাকে। তবে base R শেখাও জরুরি। কারণ base R বুঝলে R-এর ভিতরের logic ভালো বোঝা যায়। আর dplyr বুঝলে বাস্তব data analysis দ্রুত ও সুন্দরভাবে করা যায়। দুটোই দরকার।
শেষ কথা
dplyr শেখা মানে শুধু নতুন package শেখা না। এটা আসলে ডেটা নিয়ে চিন্তা করার একটা পরিষ্কার পদ্ধতি শেখা।
আমরা যখন immunology dataset নিয়ে কাজ করি, তখন খুব দ্রুত অনেক প্রশ্ন আসে। কোন condition-এ B cell বেশি? কোন sample-এ T cell কম? Disease group-এ CD8 T cell fraction কেমন? Healthy sample-এ B/T ratio কি বেশি? এই প্রশ্নগুলোর উত্তর বের করতে হলে আমাদের row filter করতে হবে, column select করতে হবে, নতুন ratio বানাতে হবে, group অনুযায়ী summary বের করতে হবে। এই কাজগুলো dplyr খুব সুন্দরভাবে করতে দেয়।
তবে শুরুতে base R example-ও দেখানো দরকার। কারণ এতে বোঝা যায় যে dplyr কোনো জাদু না। একই কাজ base R দিয়েও করা যায়। dplyr শুধু কাজটাকে একটু পরিষ্কার, readable এবং organised করে দেয়।
ছোট dataset-এ পার্থক্য খুব বেশি মনে নাও হতে পারে। কিন্তু dataset বড় হলে, column বেশি হলে, sample group বেশি হলে, তখন dplyr-এর সুবিধা খুব স্পষ্ট হয়ে যায়। তখন কোড শুধু কাজ করে না, কোড পড়াও যায়। আর ভালো analysis-এর জন্য এই দুইটাই দরকার।

