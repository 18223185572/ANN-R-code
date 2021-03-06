library(AMORE)
p<-matrix(c(6977.93,24647,11356.6,9772.5,1496.92,4279.65,89.84,95.97,9194,0.6068,
            7973.37,28534,13469.77,11585.82,1618.27,5271.991,100.28,111.16,9442,0.63,
            9294.26,33272,16004.61,14076.83,1707.98,6341.86,117.78,130.22,9660,0.6314,
            10868.67,37638,18502.2,16321.46,1790.97,6849.688,134.77,125.56,9893,0.6337,
            12933.12,39436,19419.7,18052.59,1855.73,6110.941,86.04,119.81,10130,0.634,
            15623.7,44736,23014.53,20711.55,1948.06,7848.961,151.59,187.08,10441,0.6618,
            17069.2,50807,26447.38,24097.7,2006.92,9134.673,177.79,202.12,10505,0.665,
            18751.47,54095,27700.97,26519.69,2037.88,9840.205,195.18,282.05,10594,0.674,
            21169.7,60633.82,31941.45,29569.92,2211.6665,11221.01,205.5601,329.4234,10986.79,0.684065,
            23716.17,66750.29,35562.93,32993.75,2317.9223,12486.77,220.3005,398.7751,11245.69,0.694706,
            26469.74,73292.95,39458.17,36680.63,2428.5869,13849.68,235.0408,477.4204,11515.33,0.706087),11,10,byrow=T)

#对输入矩阵进行归一化处理（0到1）
b1=(p[,1]-min(p[,1]))/(max(p[,1])-min(p[,1]))  
b2=(p[,2]-min(p[,2]))/(max(p[,2])-min(p[,2]))  
b3=(p[,3]-min(p[,3]))/(max(p[,3])-min(p[,3]))  
b4=(p[,4]-min(p[,4]))/(max(p[,4])-min(p[,4])) 
b5=(p[,5]-min(p[,5]))/(max(p[,5])-min(p[,5]))  
b6=(p[,6]-min(p[,6]))/(max(p[,6])-min(p[,6]))  
b7=(p[,7]-min(p[,7]))/(max(p[,7])-min(p[,7]))  
b8=(p[,8]-min(p[,8]))/(max(p[,8])-min(p[,8])) 
b9=(p[,9]-min(p[,9]))/(max(p[,9])-min(p[,9]))  
b10=(p[,10]-min(p[,10]))/(max(p[,10])-min(p[,10]))  
p0=cbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)#归一化后的数据放入矩阵中 

#对应矩阵前8行的测试结果集
t<-c(2673.5356,2991.0529,3393.0057,3504.8229,3609.4029,4060.1257,4399.0168,4619.4102)
#第9行的实际结果
t9=4830.1315

#测试结果归一化
t0=(t-min(t))/(max(t)-min(t))

alter=1
count=0
#训练的结果测试第9行若误差在3%之内或者循环20次结束
while(abs(alter)>0.03 && count<20){
  
  #训练网络，n.neurons表示输入的参数，以及隐藏层个数，及输出结果
  net<-newff(n.neurons = c(10,10,2,1),learning.rate.global=1e-4, momentum.global=0.05,error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm")
  #<span style="line-height: 27.2px; font-family: 'Helvetica Neue', Helvetica, Tahoma, Arial, STXihei, 'Microsoft YaHei', 微软雅黑, sans-serif;">p0[1:8,]表示输入，t0[1:8]表示输出，show.step表示循环次数，n.shows表示满足结果的报告次数</span>
  result<-train(net,p0[1:8,],t0[1:8],error.criterium="LMS", report=TRUE, show.step=10000, n.shows=5)
  #测试第9行到11行
  y<-sim(result$net,p0[9:11,])
  #反归一化
  y<-y*t[8]
  #用第9行来测试训练误差，满足训练误差结束
  alter=(y[1]-t9)/t9
  
  count=count+1;
  
}

count
#输出第9行到11行预测的值
y

