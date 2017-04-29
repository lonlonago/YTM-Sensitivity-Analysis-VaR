
# 求 x 所在区间的赫米特模型，并求出 x 期限对应的收益率值,temp 是当天收益率曲线数据
hermite = function( x, temp ) {
  term = temp$B_ANAL_CURVETERM[ order( temp$B_ANAL_CURVETERM ) ]# 所有的期限，并排好序
  
  if (any(term==x)) return( temp[ which(temp$B_ANAL_CURVETERM==x), 'B_ANAL_YIELD'])
  
  x1 = term[which(term>x)][1]  #大于 x值的两个期限值
  x2 = term[which(term>x)][2]  #大于 x值的两个期限值
  x0 = rev(term[which(term<x)]) [1]  # 小于的第一个期限
  y0 = temp[which(temp$B_ANAL_CURVETERM==x0), 'B_ANAL_YIELD']
  y1 = temp[which(temp$B_ANAL_CURVETERM==x1), 'B_ANAL_YIELD']
  y2 = temp[which(temp$B_ANAL_CURVETERM==x2), 'B_ANAL_YIELD']
  
  d0 = (y1-y0)/(x1-x0)
  d1 = (y2-y1)/(x2-x1)
  
  h1 = 3*((x1-x)/(x1-x0))^2 - 2*((x1-x)/(x1-x0))^3
  h2 = 3*((x-x0)/(x1-x0))^2 - 2*((x-x0)/(x1-x0))^3
  h3 = ((x1-x)^2/(x1-x0)) - ((x1-x)^3/(x1-x0)^2)
  h4 = ((x-x0)^3/(x1-x0)^2) - ((x-x0)^2/(x1-x0))
  
  #print(c(x0,x1,x2,y0,y1,y2,d0,d1,h1,h2,h3,h4))
  y = y0*h1 + y1*h2 + d0*h3 + d1*h4
  
  return(y)
}