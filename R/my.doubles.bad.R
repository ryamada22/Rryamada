#' Score Badminton Doubles Game
#'
#' This function returns players' info from a serial record of pointing teams.
#' @param K is a vector of 1 or 2.
#' @param p is the max point of a game.
#' @keywords miscelaneous
#' @export
#' @examples
#' K <- sample(1:2,42,replace=TRUE,prob=c(0.49,0.51))
#' K <- c(1,1,1,1,1,1,2,1,2,1,2,1,1,2,1,2,2,1,1,2,2,1,1,1,2,1,2,1,1,2,1,1)
#' K <- K * (-1) + 3
#' # The score sheet for this match : http://www.badminton.or.jp/nba/shinpan/scoresheet.pdf 
#' doubles.score <- my.doubles.bad(K)
#' # ポイントチーム記録
#' doubles.score$Point.team
#' # チーム別累積ポイントログ
#' doubles.score$Points
#' # サーバーは誰か
#' doubles.score$Server.4
#' # サーバー・レシーバーの左右位置
#' doubles.score$Server.side
#' # 各チームのメンバーの位置(初期位置かその逆か)
#' doubles.score$Location

my.doubles.bad <- function(K,p=21){
	# サービスオーバー
	diff.K <- diff(K)
	if(K[1]==2){
		diff.K <- c(1,diff.K)
	}else{
		diff.K <- c(0,diff.K)
	}
	# サーバーチーム
	Serving.team <- c(1,K)
	# 得点進行と
	# サーブ権の有無別の得点内訳
	P <- matrix(0,2,length(K))
	P.server <- P.receiver <- P
	for(i in 1:length(K)){
		P[1,i] <- length(which(K[1:i]==1))
		P[2,i] <- length(which(K[1:i]==2))
		P.receiver[1,i] <- length(which(diff.K[1:i]==-1))
		P.receiver[2,i] <- length(which(diff.K[1:i]==1))
	}
	P <- cbind(rep(0,2),P)
	P.receiver <- cbind(rep(0,2),P.receiver)
	P.server <- P-P.receiver
	# 各チームのサーバはサービスオーバー累積回数(サービスオーバー累積得点)の偶奇で決まる
	Server=(P.receiver%%2)+1
	# 各チームの左右位置は、サーブ権ありでの累積得点の偶奇で決まる
	Location=((P.server%%2)*(-2)+1)
	# ２チーム４名の誰がサーバーかを算出
	Server.4 <- c()
	for(i in 1:length(Serving.team)){
		if(Serving.team[i]==1){
			tmp <- Server[1,i]
		}else{
			tmp <- 2+Server[2,i]
		}
		Server.4 <- c(Server.4,tmp)
	}
	# サーバー・レシーバーの左右サイド
	Server.side <- c()
	for(i in 1:length(Serving.team)){
		if(Server[Serving.team[i],i]==1){
			tmp <- 1
		}else{
			tmp <- -1
		}
		Server.side <- c(Server.side,Location[Serving.team[i],i]*tmp)
		
	}
	max.P <- apply(P,2,max)
	len <- min(which(max.P==p),length(K)+1)
	return(list(Point.team=K[1:len],Points=P[,1:len],Serving.team=Serving.team[1:len],Server.4=Server.4,Server.side = Server.side[1:len],Location=Location[,1:len],Server=Server[,1:len],P.server=P.server[,1:len],P.receiver=P.receiver[,1:len]))
}
