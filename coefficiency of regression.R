#calculate regression (slope)
input=read.delim("D:/demo/demo.txt",head=T,sep="\t")
data=input[,2:ncol(input)]
y=data$IC50
i=2
matrix_data=matrix(,ncol=2,nrow=ncol(input))
while (i<(ncol(data)+1)) {
  x=data[,i:i]
  i=i+1
  result_lm=lm(y~x,data)
  coefficient=result_lm$coefficients
  matrix_data[(i-1):(i-1),1:2]=coefficient
}
write.csv(matrix_data,"D:/demo/result.csv")


x=data[,2:2]

plot(y~x,data)

names(lm)

