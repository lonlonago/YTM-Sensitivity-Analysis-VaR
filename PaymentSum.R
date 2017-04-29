# 对现金流进行计算   20161213
PayentSum = function( bondinfo) {
  
  M = as.numeric(bondinfo[1])
  i = as.numeric(bondinfo[2])
  f_month = as.numeric(bondinfo[3])
  first_day = as.character(bondinfo[4])
  last_day = as.character(bondinfo[5])
  flag = as.numeric(bondinfo[6])
  
  
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day)    # 到期日
  
  i = i / 100                 # 利率除以100
  
  if (flag==0 | flag==3 |flag==4 )  f_month = 12 # 一次还本付息、零息，贴现的理论付息周期为1年
  f =  as.integer (12/f_month)  # 转化计息月数为每年的频次
  count_day = count_date(f,first_day,last_day)   #调用计算付息日的函数
  N = length( count_day )  # 总的付息次数+1
  
  FV = M*i*(N-1) + M  # 对超短或者日期不等的一次还本付息就不是这样计算了
  
  if(  ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) { #超短必然日或月不等
    years = floor( julian( last_day, first_day)/365) #总共的年数
    daynum = julian( count_day[2], first_day) #起息日到第一次计息日的天数，因为日期的不规则，导致它不是正常的 TY
    FV = daynum/365*M*i + M*i*years + M
  }
  
  if ( flag==0 ) {               #一次还本付息债券[0]
    principle_flow = cbind(as.character(count_day[length(count_day)]) , FV)  # append(rep(0, N-2),FV)  
    
  } else if (flag==2 | flag==1){ #固定附息债[2] 浮息债1
    principle_flow = append(rep(M*i/f, N-2),M+M*i/f)
    principle_flow = cbind(as.character(count_day[-1]),principle_flow)
    
  } else if (flag==4 | flag==3){ #贴现债券[4]、 零息债券[3]
    principle_flow = cbind(as.character(count_day[length(count_day)]) , M) #append(rep(0, N-2),M)
    
  }
  # 更新为只返回不为0 的现金流
  return( principle_flow )  #cbind(as.character(count_day[-1]) , principle_flow)
  
}


# --------------计算各个计息日的具体日期的函数-----------
count_date  <- function(f, first_day, last_day) {
  
  first_day = ymd(first_day)  # 发行日期
  last_day = ymd(last_day) 
  f_month =  as.integer (12/f)  # 转化为计息月份数
  
  rap = c(184,  184 , 181, 182, 181, 182, 181, 181, 184, 183, 184, 183)  # 年付息2次的实际天数
  
  count_day1 = seq(from=first_day, to=last_day, by="year")  # 如何给半年的算
  
  if ( (day(first_day) == day(last_day)) & (month(first_day) == month(last_day)) ){
    #    日和月都相等才用这个逻辑
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
      print('计息周期没有考虑这种情况！')
      frequence = paste(f_month,'month')
      count_day = seq(first_day,last_day ,by=frequence)
    }
  } else if(  ( day(first_day) != day(last_day)) | (month(first_day) != month(last_day)) ) {
    # 日期不相等或者月份不相等就按照这个规则来生成计息日
    count_day = c()
    count_day = append(count_day,last_day)
    
    temp = last_day
    while ( temp > first_day ){
      frequence = paste(-f_month,'month')
      temp = seq(temp, ymd('19800101'),by=frequence)[2] #temp = temp - months(f_month)
      count_day = append(count_day,temp) # 会一直减少到起息日的前面一个周期日期，但是顺序好像反了
    }
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
  
  count_day = count_date(f=1,first_day,last_day) # 不论 f为多少，都以1年为频次来计算
  
  m =  length( which( julian(count_day,today) > 0 )) - 1  # 剩余年数
  next_count = count_day[ which( julian(count_day,today) > 0 )[1] ]
  
  N = length(count_day) - 1    # 总的付息次数
  n = length( which( julian(count_day,today) > 0 ))  # 剩余计息次数 
  TY = julian( count_day[ N-n+2 ], count_day[ N-n+1 ])  # 这个付息周期的实际天数
  
  d = julian(next_count,today)[1] #+ 1    # 距离下一次计息日有多少天  
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

month = function(x){ #提取日期里面的月份数
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