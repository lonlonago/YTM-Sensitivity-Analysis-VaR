#计算关键期限久期的程序 ,改变了估值和计算到期收益率的主程序，需要注意
#前面部分是计算关键期限久期的流程，还没有全部完成

setwd("/Users/lon/bonddata")
bondcurve = read.table('1231.txt',header = F,stringsAsFactors=F,col.names=c("OBJECT_ID",
                                                                            "TRADE_DT",	"B_ANAL_CURVENUMBER",	"B_ANAL_CURVENAME",	"B_ANAL_CURVETYPE",	"B_ANAL_CURVETERM",	
                                                                            "B_ANAL_YIELD",	"OPDATE","OPMINIT",	"OPMODE"))[ , c(2,6,7) ]

temp = bondcurve[ bondcurve$TRADE_DT == 20170209 , ]
model = splinefun(temp$B_ANAL_CURVETERM, temp$B_ANAL_YIELD )#, method = 'monoH.FC')  




# 这个最大期限应该是从债券的待偿期得到的，而不是从该曲线得到的
max_term = matu(today='20170209',first_day='20130523', last_day="20230523")
#max(temp$B_ANAL_CURVETERM) #原有曲线的最大期限

key_term = c(0.08, 0.25, 0.5, 1, 3, 5, 7, 10, 30, 50) #关键期限
max_indc = which( key_term >= max_term )[1]  #关键期限大于曲线的最大期限的位置
key_term_ask = key_term[2:max_indc]  # 要求的关键期限久期的期限的向量

D_key = c()
for( j2 in 1:length(key_term_ask)  ){ # length(key_term_ask)
  ask_term = key_term_ask[j2] #要求的关键期限久期的期限的第一个,后续要循环的 ask_indc = which( temp$B_ANAL_CURVETERM==ask_term) #原有曲线中等于要求的关键期限的位置
  
  # 曲线上升和下降的情况
  ask_temp = temp #最后返回的收益率下降变化了的曲线数据
  ask_temp[ask_temp$B_ANAL_CURVETERM==ask_term,"B_ANAL_YIELD"] = temp[temp$B_ANAL_CURVETERM==ask_term,"B_ANAL_YIELD"] - 1 #把要改变的关键期限的收益率减去1
  
  ask_temp_up = temp #最后返回的收益率上升变化的曲线数据
  ask_temp_up[ask_temp_up$B_ANAL_CURVETERM==ask_term,"B_ANAL_YIELD"] = temp[temp$B_ANAL_CURVETERM==ask_term,"B_ANAL_YIELD"] + 1 #把要改变的关键期限的收益率加1
  
  ask_key_indc = which(key_term==ask_term)
  up_term = key_term[ask_key_indc + 1]
  down_term = key_term[ask_key_indc - 1]
  
  change_indc = which(temp$B_ANAL_CURVETERM>down_term & temp$B_ANAL_CURVETERM<ask_term) #对关键期限下半部分进行重新设定收益率
  for(j in 1:length(change_indc)){ #相对于0.08来说，越大减去的越多，但是在上半部分，相对于0.5，越小减去的越多
    ask_temp[change_indc[j],'B_ANAL_YIELD'] = temp[change_indc[j],'B_ANAL_YIELD'] - (temp[change_indc[j],'B_ANAL_CURVETERM']-down_term)/(ask_term-down_term)
    ask_temp_up[change_indc[j],'B_ANAL_YIELD'] = temp[change_indc[j],'B_ANAL_YIELD'] + (temp[change_indc[j],'B_ANAL_CURVETERM']-down_term)/(ask_term-down_term)
  }
  
  change_indc = which(temp$B_ANAL_CURVETERM>ask_term & temp$B_ANAL_CURVETERM< up_term)#对关键期限上半部分进行重新设定收益率
  for(j in 1:length(change_indc)){
    if (is.na(up_term)) break  #上半部分如果不需要改变，跳出循环
    ask_temp[change_indc[j],'B_ANAL_YIELD'] = temp[change_indc[j],'B_ANAL_YIELD'] - (up_term - temp[change_indc[j],'B_ANAL_CURVETERM'])/(up_term-ask_term)
    ask_temp_up[change_indc[j],'B_ANAL_YIELD'] = temp[change_indc[j],'B_ANAL_YIELD'] + (up_term - temp[change_indc[j],'B_ANAL_CURVETERM'])/(up_term-ask_term)
    
  }
  
  x=seq(0.01,50,0.01)
  ask_model = splinefun(ask_temp$B_ANAL_CURVETERM, ask_temp$B_ANAL_YIELD)# , method = 'monoH.FC')  
  plot(x,ask_model(x),type='l')
  
  model = splinefun(temp$B_ANAL_CURVETERM, temp$B_ANAL_YIELD)# , method = 'monoH.FC')  
  plot(x,model(x),type='l')
  
  ask_model_up = splinefun(ask_temp_up$B_ANAL_CURVETERM, ask_temp_up$B_ANAL_YIELD)# , method = 'monoH.FC')  
  plot(x,ask_model_up(x),type='l')
  
  V0 = YTM_COUNT_FIXED(r=0, today='20170209', M=100, i=3.38, model = model,
                       f_month = 6, first_day='20130523', last_day="20230523",
                       flag = 2  )[2]  
  
  V_up = YTM_COUNT_FIXED(r=0, today='20170209', M=100, i=3.38, model = ask_model_up,
                         f_month = 6, first_day='20130523', last_day="20230523",
                         flag = 2  )[2]
  
  V_down = YTM_COUNT_FIXED(r=0, today='20170209', M=100, i=3.38, model = ask_model,
                           f_month = 6, first_day='20130523', last_day="20230523",
                           flag = 2  )[2]
  D_key[j2] = ((V_down - V_up)/(2*V0*1))*100 # 因为这里得到的是比例，是小数，所以要得到百分数就要乘以100
  print(c(ask_term,V_up,V0,V_down,D_key ))
  
}
names(D_key) = key_term_ask #把关键期限久期的名字以关键期限命名





YTM_COUNT_FIXED = function(r=0, price=0, net_price=0, issue_price=0, cash_flow=NULL, model=NULL,
                           M=100, i=0, f_month=12, s=0, 
                           principle_flow=NULL, i_change=NULL, 
                           today, first_day, last_day, flag) {
  # r 是到期收益率(用不除以100的数值)；    
  # M 是债券面值  默认值100
  # i 是票面利率
  # f 付息频次（每年的次数，有1次，2次，4次：季度 ，12次：按月付）
  # s 浮息债的招标利差
  # first_day,last_day 债券计息日和到期日，today 是交割日的日期 （日期格式为yyyy/mm/dd）
  # price 债券交割价格 ；全价
  # net_price 债券净价
  # principle_flow   债券的本金流   （对于分期还本付息的情况，长度等于付息次数，格式为principle_flow = c( 0,0,20,20,20,20,20)）
  # cash_flow  本息和的现金流
  # model 是估值日的即期利率曲线的模型，传入待偿期可以求得该期限的即期利率，
  # i_change 票面利率的改变，(是改变的值，不是改变后的值)和 i,r 一样是百分数单位，长度等于付息次数，格式为 c(0,0,....x,x,x)
  # flag 债券类型  包含         固定附息债[1]，贴现债券[4]、 零息债券[3]、到期一次还本付息债券[0]
  #                           （浮息债[2]， 含权债[6]，可转债[7] 另说）
  
  
  if ( flag==0 ) {              #一次还本付息债券[0]
    result = BOND_COUNT_ONCE(r=r, price=price, net_price=net_price, model=model,
                             M=M, i=i, f_month=f_month, 
                             today=today, first_day=first_day, last_day=last_day) 
    
  } else if (flag==2 | flag==1){ #固定附息债[2] 、浮息债1
    result = BOND_COUNT_FIX_FLOAT(r=r, price=price, net_price=net_price, cash_flow=cash_flow, model=model,
                                  issue_price=issue_price, M=M, i=i, f_month=f_month, s=s, 
                                  principle_flow=principle_flow, i_change=i_change, 
                                  today=today, first_day=first_day, last_day=last_day) 
    
  } else if (flag==4 | flag==3){ #贴现债券[4]、 零息债券[3]
    result = BOND_COUNT_ZERO (r=r, price=price, net_price=net_price, model=model,
                              issue_price=issue_price, M=M,
                              today=today, first_day=first_day, last_day=last_day)
  }
  
  return(result)
  
}


#------------固息债和浮息债的计算-------------------
BOND_COUNT_FIX_FLOAT = function(r=0, price=0, net_price=0,issue_price=0, M=100, i, f_month, 
                                s=0,cash_flow=NULL, model=model,principle_flow=NULL,
                                i_change=NULL, today, first_day, last_day) {
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  if (first_day > today) print('计算日期小于起息日，错误01！')
  
  i = i / 100                 # 利率除以100
  r = r / 100
  
  f =  as.integer (12/f_month)  # 转化计息月数为每年的频次
  
  count_day = count_date(f,first_day,last_day)   #调用计算付息日的函数
  if ( !is.null(cash_flow) )   { 
    if ( (length(count_day)-1) - length(cash_flow) == 1 ){#通过现金流判断付息日的计算是否正确
      count_day = count_day[-1]
      print('警告！多了一个计息日。')  #多了两次付息日，正常情况只会多一个起息日
      #出现在债券起息日和付息日不等，并且第一个付息日不付息的情况下，
      if (count_day[1] > today) { #有可能会导致起息日在交割日的后面，会出现错误
        print('警告！计算日期小于起息日，现金流被设置为空！')
        cash_flow = NULL
        count_day = count_date(f,first_day,last_day)   #调用计算付息日的函数
      }
    }else if( (length(count_day)-1) - length(cash_flow) != 0 ){
      cash_flow = NULL
      print('警告！计息日和现金流的长度仍然不相等！现金流被设置为空！')
    }
  }
  
  
  
  n = length( which( julian(count_day,today) > 0 ))  # 剩余计息次数
  N = length( count_day )  # 总的付息次数+1
  next_count = count_day[ which( julian(count_day,today) > 0 )[1] ]   # 下一个计息日的日期
  d = julian(next_count,today)[1] #+ 1    # 距离下一次计息有多少天 
  if (is.na(next_count)) d=0
  
  TY = julian( count_day[ N-n+1 ], count_day[ N-n ])  # 这个付息周期的实际天数
  
  MATU = matu(today, first_day, last_day)  # 待偿期   MATU = d/TY + n - 1  #这里对付息次数为2次或以上的会有错误！！！ 不是加 n-1,而是剩下的年数
  
  if ( is.null(principle_flow) )   principle_flow = append(rep(0, N-2),M)  #如果不传入本金流，那么本金流就是它；和count_day 一样长
  if ( is.null(i_change) )   i_change = rep(0,N-1) # 如果不传入票面利率的改变值，那么它为0
  
  
  PV = 0
  PV_cash_flow = 0
  YTM_cash_flow = 9999  #基础值，作为标记
  delta = 0    #一阶导和二阶导的初始值
  delta2 = 0
  for (j in 1:n) {     # 用 r 计算价格，实质上是 n 次计息的折现
    reduce_M = sum(principle_flow[1:(N-n-2+j)] )    # 对于分期兑付的债券，面值会减少 # temp = (M*i/f + principle_flow[N-1-n+j] )/(1 + r/f)^(d /TY + j - 1)     # 利息加了/ f
    
    if ( !is.null(model) )   { # 如果传入了即期利率曲线模型，就用这个模型来计算到期收益率 r
      dura = MATU -(n-1)/f + (j-1)/f  #MATU - (j-1)/f    # (d /TY + j - 1) 之所以要 n-1,因为最后一次付息不计时间  6.2845-(n-1)/f + (j-1)/f #
      r =  (model(dura))/100     #hermite(matu[j],temp )/100    -0.0238
      print(c(dura,r,n,f,MATU))
    }
    
    PV = PV + ( (M-reduce_M) *(i+i_change[N-1-n+j])/f + principle_flow[N-1-n+j] )/(1 + r/f)^(d /TY + j - 1)     #  d * f/TY  改成 TY 后不需要乘以 f
    
    if ( !is.null(cash_flow) )   { 
      PV_cash_flow =  PV_cash_flow + cash_flow[N-n-1+j]/(1 + r/f)^(d /TY + j - 1) 
    }
    
    # 计算久期，凸性等，都是根据 r来计算的，得到 PV 后再进一步计算
    ij = i_change[N-1-n+j]       #利率流
    pj = principle_flow[N-1-n+j] #本金流
    pv_formu = expression(( (M-reduce_M) *(i+ij)/f + pj )/(1 + r/f)^(d /TY + j - 1))
    d_f = D(pv_formu, 'r')
    delta = delta+eval(d_f)          # 代入 r 求导数值
    delta2 = delta2+eval(D(d_f,'r')) #求二阶导
  }
  
  Dm = -delta/PV    # 修正久期
  C = delta2/PV   # 凸性
  D = Dm*(1+r)      # 麦考利久期  
  BP_value = Dm * PV / 10000  # 基点价值         
  
  #------------------对处于最后付息周期的 固定附息和浮息 债券，按照单利-----------------------------------------------------------------------  
  if ( n==1 | today == last_day ){    #对处于最后付息周期的 固定附息和浮息 债券，按照单利
    if( julian( last_day, first_day)<365 ){
      TY = get_end_year_days( last_day ) # 365  # 如果整个周期小于1年，那么 TY 就用365;改成用到期日的年的天数了
    } else {
      TY = julian( count_day[ N ], count_day[ N-f ])  # 最后周期的计算有点不同
    }
    
    FV = ((M-reduce_M) *(i)/f) + M  # M*i/f + M  #最后一次现金流，固息债要考虑付息频率    （没有考虑到本金兑付，浮动利率！！！）
    
    AI = ((M-reduce_M) *(i)/f)*(TY-d)/TY  #  +i_change[N-1-n+j]
    if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价，分期兑付还要改！
    
    if ( !is.null(model) )   { # 如果传入了即期利率曲线模型，就用这个模型来计算到期收益率 r
      dura = MATU #- (j-1)/f   # (d /TY + j - 1) 
      r =  model(dura)/100  #hermite(matu[j],temp )/100  
      print(r)
    }
    
    YTM = (FV/price - 1)*TY/d       #计算出的到期收益率
    PV = FV / (1 + r*d/TY)
    
    if ( !is.null(cash_flow) )   {  #如果有现金流数据，就用现金流数据计算
      FV = cash_flow[length(cash_flow)] 
      PV_cash_flow = FV / (1 + r*d/TY)
      YTM_cash_flow = (FV/price - 1)*TY/d       #计算出的到期收益率
    }
    
    #print( c( "最后周期：应计利息" , AI,d,N,n,TY,price,FV,f ))   #应计利息
    #print(count_day)
    
    # 通过r计算久期，凸性  ; 其实这里是不是应该就和零息，贴现一样，直接使用待偿期了？不用计算了！
    pv_formu = expression(FV / (1 + r*d/TY))
    d_f = D(pv_formu, 'r')
    delta = eval(d_f)  # 代入 r 求导数值
    delta2 = eval(D(d_f,'r')) #求二阶导
    
    Dm = -delta/PV    # 修正久期
    C = delta2/PV   # 凸性
    D = Dm*(1+r)      # 麦考利久期  
    BP_value = Dm * PV / 10000  # 基点价值 
    
    if( !is.null(cash_flow) ){   #  一起写在后面会出问题，不能用；隔开写在一起
      PV=PV_cash_flow
      YTM=YTM_cash_flow # 有现金流的情况 
    }
    
    if ( today == last_day) { #针对到期日的计算
      PV = ifelse(is.null(cash_flow),100,cash_flow[length(cash_flow)] )      
      return( c(YTM=i*100, PV=PV, 净价=PV-AI, AI=AI,修正久期=0,麦考利久期=0,凸性=0,基点价值=0) )
    }
    
    return( c(YTM=YTM*100, PV=PV, 净价=PV-AI, AI=AI,修正久期=Dm,麦考利久期=D,凸性=C,基点价值=BP_value) )
    #,PV_cash_flow=PV_cash_flow,YTM_cash_flow=YTM_cash_flow*100
  }
  
  #------------对处于最后付息周期的 固定附息和浮息 债券，按照单利-------------------------------------------------------------------------------------  
  
  #----------计算 YTM 部分----------
  AI = ((M-sum(principle_flow[1:(N-n-2+1)] )) *(i)/f)*(TY-d)/TY  #应计利息
  PV_count = function( yield ){  #用 实际价格或净价 计算 到期收益率 ，因为求根必须用函数
    if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价
    P = - price
    for (j in 1:n) {  # n次计息的公式        
      reduce_M = sum(principle_flow[1:(N-n-2+j)] )   # temp = (M * i / f + principle_flow[N-1-n+j] )/(1 + yield/f)^(d /TY + j - 1)     # 利息加了/ f
      P = P + ( (M-reduce_M)  * (i+i_change[N-1-n+j]) / f + principle_flow[N-1-n+j] )/(1 + yield/f)^(d /TY + j - 1)  # temp[1]   #  d * f/TY  改成 TY 后不需要乘以 f
      
    }
    return(P)   # 如果没有显式的返回，就必须在最后一行返回一个值   #  P + M/(1 + yield/f)^(d /TY + j - 1)  -  price    #  * f
  }
  
  YTM = ifelse(price!=0 | net_price!=0, uniroot(PV_count , lower = -1, upper = 3, tol = 1e-10,extendInt = 'yes' )$root, 0)
  print( c("f",f,"price",price,"TY",TY,'d:',d,"N:",N,reduce_M,n) )
  print( c( "应计利息" , AI,d,N,n,TY,price,as.character(count_day) ))   #应计利息
  
  # 存在全价或净价，才可以计算 YTM，不然就是0
  if ( !is.null(cash_flow) )  {
    AI = ((M-sum(cash_flow[1:(N-n-2+1)] )) *(i)/f)*(TY-d)/TY  
    #应计利息,这里是有问题的，但是没有本金流，只有现金流。。坑，!!!!
    #所以只能用全价来算 YTM，不能用净价算（在传入现金流的情况下）
    
    PV_count = function( yield ){  #用 实际价格或净价 计算 到期收益率 ，因为求根必须用函数
      if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价
      p_cash_flow = - price
      for (j in 1:n) {  # n次计息的公式        
        p_cash_flow = p_cash_flow + cash_flow[N-1-n+j]/(1 + yield/f)^(d /TY + j - 1)  # temp[1]   #  d * f/TY  改成 TY 后不需要乘以 f
      }
      return(p_cash_flow)
      # 如果没有显式的返回，就必须在最后一行返回一个值   #  P + M/(1 + yield/f)^(d /TY + j - 1)  -  price    #  * f
    }
    
    YTM_cash_flow = ifelse(price!=0 | net_price!=0, uniroot(PV_count , lower = -1, upper = 3, tol = 1e-10,extendInt = 'yes' )$root, 0)
  }
  
  
  if( !is.null(cash_flow) ){   #  一起写在后面会出问题，不能用；隔开写在一起
    PV=PV_cash_flow
    YTM=YTM_cash_flow # 有现金流的情况 
  }
  
  
  return( c(YTM=YTM[1]*100, PV=PV, 净价=PV-AI, AI=AI,修正久期=Dm,麦考利久期=D,凸性=C,基点价值=BP_value) )
  #,PV_cash_flow=PV_cash_flow,YTM_cash_flow=YTM_cash_flow*100
}



#---------------零息，贴现债券的计算--------------------
BOND_COUNT_ZERO = function(r=0, price=0, net_price=0, issue_price, M,model=model,
                           today, first_day, last_day) { 
  
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  if (first_day > today) print('计算日期小于起息日，错误！')
  
  r = r/100  # YTM      # i=0  # 贴现债券、 零息债券
  if ( !is.null(model) )   { # 如果传入了即期利率曲线模型，就用这个模型来计算到期收益率 r
    dura = matu(today, first_day, last_day) #MATU - (j-1)/f   # (d /TY + j - 1) 
    r =  model(dura)/100  #hermite(matu[j],temp )/100  
    print(r)
  }
  
  FV = 100  # 最终本息和
  
  count_day = count_date(f=1,first_day,last_day)  # 虽然这些债券都不会实际付息，但是要计算理论付息日
  N = length(count_day) - 1    # 总的付息次数
  n = length( which( julian(count_day,today) > 0 ))  # 剩余计息次数
  
  TY = julian( count_day[ N-n+2 ], count_day[ N-n+1 ])  # 这个付息周期的实际天数
  if (julian(last_day,first_day)<365)  TY = get_end_year_days( last_day ) 
  # 如果是小于1年的债券，使用这一年的天数，而不是付息周期的天数
  
  T_total = julian(last_day,first_day)[1] # 总天数
  AI = (M - issue_price)*julian(today,first_day)[1] / T_total  #应计利息
  
  if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价
  
  if (julian(last_day,today) <= 365  ){  #对 待偿期在一年以下的贴现债券、 零息债券  #按照单利
    
    d = julian(last_day,today)[1] #+ 1    # 距离到期日有多少天 
    #print( c("FV",FV,"price",price,"TY",TY,'t_total',T_total,'d:',d,'n=:',n) )
    
    YTM = ( (FV/price - 1)*TY/d ) #计算出的到期收益率 如果 price 为0则 YTM 为 INF
    PV = FV / (1+r*d/TY)          # 现值  如果 r 为0则 PV 为 FV
    
    # 通过r计算久期，凸性
    pv_formu = expression(FV / (1 + r*d/TY))
    d_f = D(pv_formu, 'r')
    delta = eval(d_f)  # 代入 r 求导数值
    delta2 = eval(D(d_f,'r')) #求二阶导
    
    Dm = -delta/PV    # 修正久期
    C = delta2/PV   # 凸性
    D = Dm*(1+r)      # 麦考利久期  
    BP_value = Dm * PV / 10000  # 基点价值 
  }
  
  
  if (julian(last_day,today) > 365  ){ #对 待偿期大于1年的贴现债券、零息债券   按照复利
    
    m = floor( julian(last_day,today)/365 )   # 剩余的整年数
    next_count = count_day[ which( julian(count_day,today) > 0 )[1] ]   # 下一个计息日的日期
    d = julian(next_count,today)[1] #+ 1    # 距离下一次理论计息日有多少天  这里不会实际计息   
    # 这里的TY-d会导致一点问题，在 t不是从上一个计息日开始的情况下
    #print( c("FV",FV,"price",price,"YT",TY,'d:',d,"m:",m,'m=n?:',n) )
    
    YTM = ( FV/price )^(1/(n+d/TY)) - 1  # 计算到期收益率
    PV = FV / ( (1+r)^(n+d/TY) )         # 现值
    
    # 通过r计算久期，凸性
    pv_formu = expression(FV / ( (1+r)^(n+d/TY) ))
    d_f = D(pv_formu, 'r')
    delta = eval(d_f)  # 代入 r 求导数值
    delta2 = eval(D(d_f,'r')) #求二阶导
    
    Dm = -delta/PV    # 修正久期
    C = delta2/PV   # 凸性
    D = Dm*(1+r)      # 麦考利久期  
    BP_value = Dm * PV / 10000  # 基点价值
  }
  
  if ( today == last_day) { #针对到期日的计算
    return( c(YTM=0, PV=100, 净价=100-AI, AI=AI,修正久期=0,麦考利久期=0,凸性=0,基点价值=0) )
  }
  return( c(YTM=YTM[1]*100, PV=PV, 净价=PV-AI, AI=AI,修正久期=Dm,麦考利久期=D,凸性=C,基点价值=BP_value) )
  
}



#--------一次还本付息的债券计算-------------
BOND_COUNT_ONCE = function(r=0, price=0, net_price=0, M, i,f_month=0, model=model,
                           today, first_day, last_day) {
  
  #print('一次还本付息计算')
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  if (first_day > today) print('计算日期小于起息日，错误！')
  
  r = r/100 
  if ( !is.null(model) )   { # 如果传入了即期利率曲线模型，就用这个模型来计算到期收益率 r
    dura = matu(today, first_day, last_day) #MATU - (j-1)/f   # (d /TY + j - 1) 
    r =  model(dura)/100  #hermite(matu[j],temp )/100  
    print(r)
  }
  
  i = i/100
  
  # 一次还本付息的会不会出现一年多次计息的情况 ？ 一定为0
  # if (f_month!=0)   f =  as.integer (12/f_month)  # 转化计息月数为每年的频次
  count_day = count_date(f=1,first_day,last_day)  # 虽然这些债券中间不会实际付息，但是要计算理论付息日
  
  N = length(count_day) - 1    # 总的付息次数
  n = length( which( julian(count_day,today) > 0 ))  # 剩余计息次数
  TY = julian( count_day[ N-n+2 ], count_day[ N-n+1 ])  # 这个付息周期的实际天数
  if ( julian(last_day,first_day)<365 )  TY = get_end_year_days( last_day ) 
  # 如果是小于1年的债券，使用这一年的天数，而不是付息周期的天数
  
  FV = M*i*N + M  # 对超短或者日期不等的一次还本付息就不是这样计算了。。。妈的
  
  if(  ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) { #超短必然日或月不等
    years = floor( julian( last_day, first_day)/365) #总共的年数
    daynum = julian( count_day[2], first_day) #起息日到第一次计息日的天数，因为日期的不规则，导致它不是正常的 TY
    FV = daynum/365*M*i + M*i*years + M
  }
  
  if (julian(last_day,today) <= 365  ){  #对 待偿期在一年以下的到期一次还本付息债券  按照单利
    
    d = julian(last_day,today)[1] #+ 1    # 距离到期日有多少天, 因为肯定是最后一个周期了
    # 应计利息  分零息和一次还本付息分别计算
    t = ( TY-d ) #该周期已经计息的天数
    if( julian(last_day,first_day)<365 |
        ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) {
      # t = julian(today,first_day) #超短债
      t =  ( TY-d-(julian(first_day,count_day[1])) ) 
      # 已经计息的天数减去 （起息日和 count_day 之间的误差天数），
      # 可以对总时长大于1年的不规则日期的债券的计息天数 做出比较准确的计算
    }
    AI =  (N-n) * M*i + M*i*t/TY  # 对于大于1年，且不规则日期的债券还是不能准确计算。。。。
    if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价
    
    YTM = ( (FV/price - 1)*TY/d ) #计算出的到期收益率 如果 price 为0则 YTM 为 INF
    PV = FV / (1+r*d/TY)          # 现值  如果 r 为0则 PV 为 FV
    
    #print(c(FV,price,TY,AI,N,n,d,r))
    
    # 通过r计算久期，凸性
    pv_formu = expression(FV / (1+r*d/TY))
    d_f = D(pv_formu, 'r')
    delta = eval(d_f)  # 代入 r 求导数值
    delta2 = eval(D(d_f,'r')) #求二阶导
    
    Dm = -delta/PV    # 修正久期
    C = delta2/PV   # 凸性
    D = Dm*(1+r)      # 麦考利久期  
    BP_value = Dm * PV / 10000  # 基点价值
  }
  
  
  if (julian(last_day,today) > 365  ){ 
    #对 待偿期大于1年的 到期一次还本付息债券   按照复利
    m = floor( julian(last_day,today)/365 )   # 剩余的整年数
    next_count = count_day[ which( julian(count_day,today) > 0 )[1] ]   # 下一个计息日的日期
    d = julian(next_count,today)[1] #+ 1    # 距离下一次理论计息日有多少天  这里不会实际计息               这里的TY-d会导致一点问题，在 t不是从上一个计息日开始的情况下
    
    t = (TY-d)
    if(  ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) {
      #if (today < count_day[2])  { t = julian(today,first_day) # 处于不规则日期的第一个周期 } 
      t = ( TY-d-(julian(first_day,count_day[1])) ) # 处于不规则日期的大于第一个付息日的周期, 和上面一句结果一样
    }
    AI =  (N-n) * M*i + M*i*t/TY
    if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价
    
    YTM = ( FV/price )^(1/(m+d/TY)) - 1  #计算出的到期收益率
    PV = FV / ( (1+r)^(m+d/TY) )         # 现值
    
    # 通过r计算久期，凸性
    pv_formu = expression(FV / ( (1+r)^(m+d/TY) ))
    d_f = D(pv_formu, 'r')
    delta = eval(d_f)  # 代入 r 求导数值
    delta2 = eval(D(d_f,'r')) #求二阶导
    
    Dm = -delta/PV    # 修正久期
    C = delta2/PV   # 凸性
    D = Dm*(1+r)      # 麦考利久期  
    BP_value = Dm * PV / 10000  # 基点价值
  }
  
  if ( today == last_day) { #针对到期日的计算
    return( c(YTM=i*100, PV=FV, 净价=FV-AI, AI=AI,修正久期=0,麦考利久期=0,凸性=0,基点价值=0) )
  }
  return( c(YTM=YTM[1]*100, PV=PV, 净价=PV-AI, AI=AI,修正久期=Dm,麦考利久期=D,凸性=C,基点价值=BP_value) )
  
}


