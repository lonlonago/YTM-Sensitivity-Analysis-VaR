# 20160118  修改到期日的全价和 YTM 计算，增加了日志和错误捕获语句
# 20160116  修改计算流程，使得投组和资产类型的计算合并在整体的计算过程中，减少计算时间2/3
# 20161207 修改债券信息为数据框，可以返回每个债券的 VaR，可以检测并删除空债券；计算完成后清除变量
# 20161201  张龙



#------------固息债和浮息债的计算-------------------
BOND_COUNT_FIX_FLOAT = function(r=0, price=0, net_price=0,issue_price=0, M=100, i, f_month, s=0,cash_flow=NULL,
                                principle_flow=NULL, i_change=NULL, today, first_day, last_day) {
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
    PV = PV + ( (M-reduce_M) *(i+i_change[N-1-n+j])/f + principle_flow[N-1-n+j] )/(1 + r/f)^(d /TY + j - 1)     #  d * f/TY  改成 TY 后不需要乘以 f
    
    if ( !is.null(cash_flow) )   { 
      PV_cash_flow =  PV_cash_flow + cash_flow[N-n-1+j]/(1 + r/f)^(d /TY + j - 1) 
    }
    
    # 计算久期，凸性等，都是根据 r来计算的，得到 PV 后再进一步计算
    ij = i_change[N-1-n+j] #利率流
    pj = principle_flow[N-1-n+j] #本金流
    pv_formu = expression(( (M-reduce_M) *(i+ij)/f + pj )/(1 + r/f)^(d /TY + j - 1))
    d_f = D(pv_formu, 'r')
    delta = delta+eval(d_f)  # 代入 r 求导数值
    delta2 = delta2+eval(D(d_f,'r')) #求二阶导
  }
  
  Dm = -delta/PV    # 修正久期
  C = delta2/PV   # 凸性
  D = Dm*(1+r)      # 麦考利久期  
  BP_value = Dm * PV / 10000  # 基点价值         
  
#-----------------------------------------------------------------------------------------  
  if ( n==1 | today == last_day ){    #对处于最后付息周期的 固定附息和浮息 债券，按照单利
    if( julian( last_day, first_day)<365 ){
      TY = get_end_year_days( last_day ) # 365  # 如果整个周期小于1年，那么 TY 就用365;改成用到期日的年的天数了
    } else {
      TY = julian( count_day[ N ], count_day[ N-f ])  # 最后周期的计算有点不同
    }
    
    FV = ((M-reduce_M) *(i)/f) + M  # M*i/f + M  #最后一次现金流，固息债要考虑付息频率    （没有考虑到本金兑付，浮动利率！！！）
    
    AI = ((M-reduce_M) *(i)/f)*(TY-d)/TY  #  +i_change[N-1-n+j]
    if( net_price!=0 ) price=net_price + AI  # 如果输入净价，就使用净价，分期兑付还要改！
    
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

#-------------------------------------------------------------------------------------------------  
  
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
  #print( c("f",f,"price",price,"TY",TY,'d:',d,"N:",N,reduce_M,n) )
  #print( c( "应计利息" , AI,d,N,n,TY,price,as.character(count_day) ))   #应计利息
  
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
BOND_COUNT_ZERO = function(r=0, price=0, net_price=0, issue_price, M, today, first_day, last_day) { 
  
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  if (first_day > today) print('计算日期小于起息日，错误！')
  
  r = r/100  # YTM      # i=0  # 贴现债券、 零息债券
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
BOND_COUNT_ONCE = function(r=0, price=0, net_price=0, M, i,f_month=0, today, first_day, last_day) {
  
  #print('一次还本付息计算')
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  if (first_day > today) print('计算日期小于起息日，错误！')
  
  r = r/100 
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




# --------------计算各个计息日的具体日期的函数-----------
count_date  <- function(f, first_day, last_day) {
  
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day) 
  f_month =  as.integer (12/f)  # 转化为计息月份数
  
  rap = c(184,  184 , 181, 182, 181, 182, 181, 181, 184, 183, 184, 183)  # 年付息2次的实际天数
  
  count_day1 = seq(from=first_day, to=last_day, by="year")  # 如何给半年的算
  
  if ( (day(first_day) == day(last_day)) & (month(first_day) == month(last_day)) ){
    # 日和月都相等才用这个逻辑
    if(f == 2){   # 计息次数为2 时，各个付息日的日期
      m = month( first_day + 182)   # 下一个付息日所在月份 (这里存在一定疑问，也可以加183，不确定？？
      first_count =first_day + rap[m]  # 第一个计息日
      count_day2 = seq(from=first_count, to=last_day, by="year")  # 半年期的计息日
      count_day = count_day1
      k = 1
      for (j in 1:length(count_day2)) {
        count_day[k] = count_day1[j]
        k = k + 1
        count_day[k] = count_day2[j]
        k = k + 1
      }
      count_day[k] = count_day1[j+1]
    } else if( f==1 ) {
      count_day = count_day1
    } else if( f==4 ) {
      count_day = seq(first_day,last_day ,by="quarters")
    } else if( f==12 & day(first_day) == day(last_day) ){
      count_day = seq(first_day,last_day ,by="month")
    } else  { 
      #print('计息周期没有考虑这种情况！')
      n_month = paste(f_month,'month')
      count_day = seq(first_day,last_day ,by=n_month)
    }
  } else if(  ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) {
    # 日期不相等或者月份不相等就按照这个规则来生成计息日
    count_day = c()
    count_day = append(count_day,last_day)
    
    temp = last_day
    while ( temp > first_day ){
      n_month = paste(-f_month,'month')
      temp = seq(temp, ymd('19800101'),by=n_month)[2]  #temp = temp - months(f_month)
      count_day = append(count_day,temp) # 会一直减少到起息日的前面一个周期日期，但是顺序好像反了
    }
    
    # 针对不规则起息日和到期日的情况，比如一个月付息一次，20151224起息日，到期日为2020126，20161226不会付息；
    # 但是，如果20150318起息日，到期日20200126，在20150326会付息一次。。
    # 因此存在在第一个倒推的付息日是否付息的问题，导致付息日怎么都不会计算准确的情况
    # if (julian( count_day[length(count_day)-1], first_day ) < 10 )  count_day = count_day[-length(count_day)]
    # 如果有第一次计息日和起息日的天数隔得很近的情况，去掉第一个计息日，把起息日调整为下一个，第二个计息日
    # 这个10天的规则是很不可靠的，有可能会导致其他未知的问题
    return( rev( count_day )) # 第一个付息日不是起息日，而是到期日减去 n 个付息周期
    # 这里两难处境，用起息日的话 TY（该周期天数）会不准确，用付息日的话 t（计息天数）不准确
  }
  
  return(count_day)
  
}


# --------------计算剩余年数的函数，不论付息周期是怎么样-----------
matu  <- function(today, first_day, last_day) {
  
  today = ymd(today)
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  
  if ( today == last_day) return(0) # 已经到期，代偿期为0
  if ( today > last_day) print('计算日期大于到期日，错误！')
  
  count_day = count_date(f=1,first_day,last_day) # 不论 f为多少，都以1年为频次来计算
  
  m =  length( which( julian(count_day,today) > 0 )) - 1  # 剩余年数
  next_count = count_day[ which( julian(count_day,today) > 0 )[1] ]
  
  N = length(count_day) - 1    # 总的付息次数
  n = length( which( julian(count_day,today) > 0 ))  # 剩余计息次数 
  TY = julian( count_day[ N-n+2 ], count_day[ N-n+1 ])  # 这个付息周期的实际天数
  
  d = julian(next_count,today)[1] #+ 1    # 距离下一次计息日有多少天
  print(c(d,TY))
  matu = m + d/TY
  return(matu[1])
}


#---------工具类函数--------
ymd = function(x){ # 把%Y%m%d格式的字符串转换为日期,没有考虑如果是数值的情况
  if( grepl('\\/', x) ){
    x = as.Date(x,format="%Y/%m/%d")
  } else if( grepl('\\-', x) ){
    x = as.Date(x,format="%Y-%m-%d")
  } else if( class(x)=='integer' | class(x)=='numeric'){
    x = as.Date(as.character(x) ,format="%Y%m%d")
  } else { ( x = as.Date(x,format="%Y%m%d") ) }
  
  return( x )
}

year = function(x){ #提取日期里面的年份数
  if (class(x)=='Date') {
    x = as.numeric( format(x,"%Y") )
  } else if( class(x) == "character" ){
    x = as.numeric( substr(x,1,4) ) #如果是字符串则取第五个和第六个字符
  }
  
  return(x)
}

month = function(x){ #提取日期里面的月份数  在日期是10个的字符串的时候会有问题！！
  if (class(x)=='Date') {
    x = as.numeric( format(x,"%m") )
  } else if( class(x) == "character" ){
    x = as.numeric( substr(x,5,6) ) #如果是字符串则取第五个和第六个字符
  }
  
  return(x)
}

day = function(x){ # 提取日期里面的天数值
  if (class(x)=='Date') {
    x = as.numeric( format(x,"%d") )
  } else if( class(x) == "character" ){
    x = as.numeric( substr(x,7,8) ) #如果是字符串则取第五个和第六个字符
  }
  
  return(x)
}


# 返回某一年有多少天
get_end_year_days = function( last_day ){
  year_num = year(last_day)
  start = ymd( paste(year_num,'0101',sep = '') )
  end = ymd( paste(year_num,'1231',sep = '') )
  days = julian(end,start)[1]
  return(days)
}

# 解析传递过来的收益率曲线的键值对的字符串，类似于下面的 cxy
termyieldProcess = function(cxy){
  # cxy='0.111:3.4,0.9877777:0.22,4.434:1.333'
  xy = strsplit(cxy,',')
  x = c()
  y = c()
  for (k in 1:length(xy[[1]])){
    temp = strsplit(xy[[1]][k],':')
    x = append(x, as.numeric( temp[[1]][1] ))
    y = append(y, as.numeric( temp[[1]][2] ))
  }
  return(cbind(x,y)) # 返回 x,y 二列数据组成的数据框
}



