# 20160118  修改到期日的全价和 YTM 计算，增加了日志和错误捕获语句
# 20160116  修改计算流程，使得投组和资产类型的计算合并在整体的计算过程中，减少计算时间2/3
# 20161207 修改债券信息为数据框，可以返回每个债券的 VaR，可以检测并删除空债券；计算完成后清除变量
# 20161201  张龙
# 计算组合和单个债券的 VaR 
# 修改了大量之前计算 VaR 的细节，比如 ytm_diff 计算，ytm_holdday 的计算，加入曲线编号和债券数量信息
# 对 VaR 值做了多次的检验，确定了差别最小的一种计算方法
#count_var("/users/lon/bonddata","2016-11-18")
#setwd("/users/lon/bonddata");today="2016-11-18"

count_var = function( data_road,today ) {

    setwd(data_road)
    sink('VaR_run_log.txt')  #,append = T,split = T)
    print(c('run VaR calculate,use',today,'data!'))
    
    # 收益率曲线历史数据 
    bond_curve = read.table('curves.txt',header=F,sep = ';',stringsAsFactors=F,col.names=
                              c("TRADE_DT","B_ANAL_CURVENUMBER",'termyield'))
    
    # 组合内的债券信息  需要传入的债券基本信息
    bond_info = read.table('bonds.txt',header=F,sep = ';',stringsAsFactors=F,col.names=
                             c("first_day","last_day","issue_price","i","f_month","flag","bond_amount",
                               "pv_today","curvenumber","bondcode",'assetstype','teamid'),
                           colClasses = c('character','character','numeric','numeric','numeric','numeric',
                                          'numeric','numeric','numeric',"character","character","character"
                                          ))#[1,]
    
    # 债券现金流信息
    bond_cf = read.table('bondcf.txt',header=F,sep = ';',stringsAsFactors=F,col.names=
                           c('id','teamid','bondcode','assetstype','bond_amount',
                             'paydate','paysum','carrydate',"TRADE_DT"),colClasses = c(
                               'character','character','character','character',
                               'numeric','character','numeric','character','character'
                             ))
    
    
    var=tryCatch( TeamVaR(bond_info , today, bond_curve ,bond_cf, hold_time=1),
                  error=function(e) {cat("Error",conditionMessage(e),"\n\n")})
    write.table(var,file = 'var.txt',col.names = F,sep = ';')

}


#[1] "processing... 1689086 k= 126"  曲线数量少于250
#[1] "processing....  k= 87   uniroot 错误   pv=4.3164    112 339 327 306  258 240 227 存在类似情况
# 1589044 到期日不对  1589296 多了一个  1689041 f=12  1589295 多了一个
# 1589052 多了一个   1689048 f=12
# [1] "processing.... 160008 k= 36 use_curve= 0"  为什么是0
#  100202  多出来了。。。
# 169927 var 值达到了1  它是国债，哪会这么大
# 111693723  
# 1589047   付息周期为1个月，  两种情况，一是贴现，而是付息周期短的有问题
# 计算单个债券和组合 VaR  031654034，1589044 到期日不对 2021-02-26
#  1589296 多了一个  1689041 f=12  1589295 多了一个
# 1589052 多了一个   1689048 f=12





# 计算单个债券和组合 VaR
TeamVaR = function( bond_info, today, bond_curve,bond_cf, hold_time=1 ) {
  # bond_info 组合内债券基本信息数据框； # bond_curve,所有涉及到的曲线一年的信息；# 交易日期和持有期
  # wt = 0    # 投组当前总市值,初始值为0
  
  n = dim(bond_info)[1]   # 投组里的债券数量
  before_250 = before_250_loc( bond_curve, today ) 
  today2 = ymd(today)
  hold_day = today2 + hold_time 
  
  ytm_holdday = c() #计算每只债券持有期之后的日期的的 ytm
  ytm_250 = c() # matrix(nrow = length(before_250), ncol = n) # 组合历史收益率的矩阵 250*n 为了让 diff 可以在每列上运行
  pv_250_n = matrix(nrow = length(before_250)-1 ,ncol = n) # 250*n; 因为 ytmdiff的原因，少了一行
  var = matrix( nrow = n ,ncol = 24 ) # 存储债券的 VaR ,每一行代表一个债券,第一个字段是债券代码,最后一个是历史值的字符串
  
  for( k in 1:n){

    first_day = bond_info[k,][1,'first_day'] #在只有一行的时候居然不加1，就是返回一个数据框。。。
    last_day = bond_info[k,][1,'last_day']
    issue_price = as.numeric(bond_info[k,][1,'issue_price'] )
    M = 100
    i = as.numeric(bond_info[k,][1,'i'])
    f_month = as.numeric(bond_info[k,][1,'f_month'])
    flag = as.numeric(bond_info[k,][1,'flag'])
    bond_amount = as.numeric(bond_info[k,][1,'bond_amount'])
    pv_today = as.numeric(bond_info[k,][1,'pv_today'])
    curvenumber = as.numeric(bond_info[k,][1,'curvenumber'])  # java 中需要转成字符串
    bondcode = bond_info[k,][1,'bondcode']
    assetstype = bond_info[k,][1,'assetstype']
    teamid = bond_info[k,][1,'teamid']
    cash_flow = bond_cf[ bond_cf$teamid==teamid & bond_cf$bondcode==bondcode & bond_cf$assetstype==assetstype, 'paysum']
    
    Matu_holdday  = matu (hold_day, first_day, last_day)# 要用持有期之后的日期计算待偿期
    Matu_today  = matu (today2, first_day, last_day)   # 今天的待偿期
    
    itscuvre = bond_curve[bond_curve$B_ANAL_CURVENUMBER==curvenumber, ] # 该债券对应的收益率曲线数据

    #curvelength = ifelse( (dim(itscuvre)[1] < length(before_250)) , dim(itscuvre)[1],length(before_250))
    if ( dim(itscuvre)[1] < length(before_250) ) {
      curvenumber_set = sort(unique(bond_info$curvenumber))
      curvenumber_loc = which(curvenumber_set==curvenumber)
      curvenumber = curvenumber_set[curvenumber_loc-1]
      itscuvre = bond_curve[bond_curve$B_ANAL_CURVENUMBER==curvenumber, ]
      # 如果该债券的曲线没有250天的数据，就用临近的曲线代替
      
      if ( dim(itscuvre)[1] < length(before_250) ) {
        itscuvre = bond_curve[bond_curve$B_ANAL_CURVENUMBER==1232, ] 
        # 如果该债券的曲线还是没有250天的数据，就用国债的代替
        print(paste(k,' curve not long enough! replace with 1032.','bondcode=',bondcode))
      }
      print(paste(k,' curve not long enough! replace with ',curvenumber,'bondcode=',bondcode))
    }
    
    temp = itscuvre[ itscuvre$TRADE_DT ==today,]
    temp2 = termyieldProcess( temp$termyield )
    model = splinefun(temp2[,1], temp2[,2])  #model = splinefun(temp$B_ANAL_CURVETERM, temp$B_ANAL_YIELD)
    ytm_hold = model(Matu_holdday)  # 持有期之后的债券的 YTM
    ytm_today = model(Matu_today) # 今天的 曲线上的YTM
    ytm_diff_hodday = ytm_today - ytm_hold # 曲线上的两个相邻的收益率之差 0.0001047684

    ytm_today_count = YTM_COUNT_FIXED( price = pv_today, today=today, M=M, i=i,issue_price=issue_price, 
                                       f_month = f_month, first_day=first_day, last_day=last_day,
                                       flag = flag ,cash_flow=cash_flow )[1]
    # 如果后面使用 today 进行计算，那这里也不用减去ytm_diff_hodday了
    ytm_holdday[k] = ytm_today_count #+ ytm_diff_hodday  #ytm_hold 持有期之后的基础到期收益率
    
    print(paste('processing....bondcode=',bondcode,'k=',k))
    
    for (j in 1:length(before_250) ){    # 过去250天的历史曲线的上的到期收益率，日期从近到远
      temp = itscuvre[ itscuvre$TRADE_DT == before_250[j] , ]
      temp2 = termyieldProcess( temp$termyield )
      model = splinefun(temp2[,1], temp2[,2])  #model = splinefun(temp$B_ANAL_CURVETERM, temp$B_ANAL_YIELD)# , method = 'monoH.FC')  
      ytm_250[j] = model( Matu_holdday )  # 持有期之后的点的历史250天的收益率
      # Matu_holdday 改成今天的点的波动了
    }
    
    ytm_diff = - diff( ytm_250,lag = hold_time ) # 用今天的减去昨天的, diff 是 后面的数据减去前面的
    
    ytm_future = ytm_holdday[k] + ytm_diff # 单个债券未来的 YTM 向量
    
    # 这里计算的日期从 hold_day 改为 today，因为有的债券如果使用hold_day会导致 PV 增大，导致 var 是正值
    for (j in 1:length(ytm_future) ){    # 过去250天的历史曲线的上的到期收益率，日期从近到远
      pv_250_n[j,k] = YTM_COUNT_FIXED(r=ytm_future[j], today=hold_day, M=M, i=i, issue_price=issue_price,
                                      f_month = f_month, first_day=first_day, last_day=last_day,
                                      flag = flag ,cash_flow=cash_flow )[2]
    }
    pv_history = paste(pv_250_n[,k],collapse = ',') #历史可能的 PV 值
    
    var[k,1:9] = c( bondcode, assetstype,teamid, bond_amount, tail_var(pv_250_n[,k], pv_today), pv_history )  # 计算单个债券的 VaR
    
    # quantile(pv_250_n[,k]-pv_today, na.rm = T, probs = c(0, 0.01, 0.05, 0.95, 0.99) )
    pv_250_n[,k] = pv_250_n[,k] * bond_amount
    
    wt = sum(bond_info$bond_amount*bond_info$pv_today) #wt + bond_amount*pv_today #总市值
    
  }
  all_pv_250 = rowSums(pv_250_n,na.rm = T) # 投组的所有可能总市值
  
  all_pv_history = paste(all_pv_250,collapse = ',')
  all_var = tail_var(all_pv_250, wt)
  var[,10] = all_var[1]
  var[,11] = all_var[2]
  var[,12] = all_var[3]
  var[,13] = all_var[4]
  var[,14] = all_pv_history
  
  for ( at in c("A", "H", "T", "D")){
    at_info = bond_info[bond_info$assetstype==at,]
    at_wt = sum(at_info$bond_amount*at_info$pv_today) #资产类型总市值
    at_pv_250_n = pv_250_n[,as.numeric( rownames(at_info) )] #资产类型所有可能市值
    if(class(at_pv_250_n)=='numeric') { # 只有一只债券的情况
      at_pv_250 = at_pv_250_n
    }else{
      at_pv_250 = rowSums(at_pv_250_n,na.rm = T) # 资产类型的所有可能总市值
    }
    at_pv_history = paste(at_pv_250,collapse = ',')
    at_var = tail_var(at_pv_250, at_wt)  # 资产类型 VaR
 
    var[as.numeric( rownames(at_info)), 15 ] = at_var[1]
    var[as.numeric( rownames(at_info)), 16 ] = at_var[2]
    var[as.numeric( rownames(at_info)), 17 ] = at_var[3]
    var[as.numeric( rownames(at_info)), 18 ] = at_var[4]
    var[as.numeric( rownames(at_info)), 19 ] = at_pv_history
  }
  
  # 所有的投组和资产类型的组合
  team_set = aggregate( cbind(teamid, assetstype) ~ teamid + assetstype,FUN = function(x) x[1],data=bond_info)[,1:2]
  
  for ( j in 1:dim(team_set)[1] ){
    team_info = bond_info[bond_info$assetstype==team_set[j,]$assetstype & bond_info$teamid==team_set[j,]$teamid,]
    team_wt = sum(team_info$bond_amount*team_info$pv_today) #投组总市值
    team_pv_250_n = pv_250_n[,as.numeric( rownames(team_info) )] #投组所有可能市值
    if(class(team_pv_250_n)=='numeric') { # 只有一只债券的情况
      team_pv_250 = team_pv_250_n
    }else{
      team_pv_250 = rowSums(team_pv_250_n,na.rm = T) # 投组的所有可能总市值
    }
    team_pv_history = paste(team_pv_250,collapse = ',')
    team_var = tail_var(team_pv_250, team_wt)  # 投组 VaR
    
    var[as.numeric( rownames(team_info)), 20 ] = team_var[1]
    var[as.numeric( rownames(team_info)), 21 ] = team_var[2]
    var[as.numeric( rownames(team_info)), 22 ] = team_var[3]
    var[as.numeric( rownames(team_info)), 23 ] = team_var[4]
    var[as.numeric( rownames(team_info)), 24 ] = team_pv_history
  }
  
  
 return( var )
}



#----根据未来 pv序列的尾部数据和今天的 PV 计算 VaR
tail_var = function( pv_250, pv_today){
  
  #-----------VaR计算----------------
  
  pv_threshold_5 = mean( sort(pv_250)[12:13] , na.rm = T) #sort(pv_250)[12]
  pv_threshold_1 = mean( sort(pv_250)[2:3] , na.rm = T)   #sort(pv_250)[2] 
  
  VaR_precent_5 = (pv_threshold_5 - pv_today)  
  VaR_precent_1 = (pv_threshold_1 - pv_today)  
  
  #-----------cVaR计算----------------
  
  c_pv_threshold_5 = sum( sort(pv_250)[1:13], na.rm = T)/13  #尾部的13个数据
  c_pv_threshold_1 = sum( sort(pv_250)[1:3] , na.rm = T )/3  # 尾部的3个数据
  
  cVaR_precent_5 = (c_pv_threshold_5 - pv_today)  
  cVaR_precent_1 = (c_pv_threshold_1 - pv_today)
  
  return( c(VaR5=VaR_precent_5,VaR1=VaR_precent_1,CVaR5=cVaR_precent_5,CVaR1=cVaR_precent_1)  )
}




# -----返回前面 250 天工作日的日期集合的函数--------
before_250_loc = function( bond_curve , today,n=1){  # 用 bond_price  (bond_curve) 做参数，因为都会用到
  
  day_order = order( unique( bond_curve$TRADE_DT ) , decreasing = T) #日期顺序 字符串日期也会排序
  day_set = unique( bond_curve$TRADE_DT ) [day_order]  # 日期集合  
  if (day_set[1] < today) print('曲线的最大日期小于计算日期，错误！')
  
  today_loc = which( day_set == today )   # 今天在序列中的位置 ，但是这里如果数据类型不一样会出问题
  before_250 = day_set[(today_loc):(today_loc+250)]  # 之前250天的日期序列
  if( any(is.na(before_250)) ) print('查找当前日期前250个工作日失败！')
  return(before_250)
}




