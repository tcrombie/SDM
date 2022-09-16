clear 
clc 
close all 

%load data
cd .. 
cd data

data = readtable('firthpca_stats_df.csv');
data_env = readtable('firth_env_stats_df.csv');

cd .. 
cd scripts

true_label = table2array(data(:,4)); %true labels
output = table2array(data(:,16)); %output from logistic regression with firth correction with PC1, PC2, PC3. 
x=1:length(output); 
figure(1)
for i=1:length(output)
    if true_label(i) == 1
plot(x(i),output(i),'o','Color','g','MarkerFaceColor','g','MarkerSize',10)
    else 
        plot(x(i),output(i),'*','Color','b','MarkerFaceColor','b','MarkerSize',10)
    end 
hold on 
end 
xlabel('Samples')
ylabel('Output Score')
set(gca,'FontSize',42);
axis([0 length(output) 0 max(output)])
title('Logistic Regression Firth on PC1, PC2, and PC3')

true_label_env = table2array(data_env(:,4)); %true labels
output_env = table2array(data_env(:,16)); %output from logistic regression with firth correction with PC1, PC2, PC3. 
x=1:length(output_env); 

figure(2)
for i=1:length(output_env)
    if true_label_env(i) == 1
plot(x(i),output_env(i),'o','Color','g','MarkerFaceColor','g','MarkerSize',10)
    else 
        plot(x(i),output_env(i),'*','Color','b','MarkerFaceColor','b','MarkerSize',10)
    end 
hold on 
end 
xlabel('Samples')
ylabel('Output Score')
set(gca,'FontSize',42);
axis([0 length(output_env) 0 max(output_env)])
title('Logistic Regression Firth on Environmental Variables')



