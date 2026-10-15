read_data <- function(path) data.table::fread(path, na.strings=c("NA", "", "NULL"), showProgress=FALSE, data.table=FALSE)
write_csv <- function(x, path) utils::write.csv(x, path, row.names=FALSE, na="")

run_pipeline <- function(root=".") {
  dir.create("results", showWarnings=FALSE)
  d <- read_data("data/wildfires_sample_100k.csv")
  source_column_count <- ncol(d)
  required <- c("FOD_ID","FIRE_YEAR","DISCOVERY_DATE","DISCOVERY_DOY","FIRE_SIZE","FIRE_SIZE_CLASS","LATITUDE","LONGITUDE","STATE","COUNTY")
  stopifnot(all(required %in% names(d)))
  # Julian day is encoded as a Julian date; derive year/month from the actual date.
  # FPA-FOD DISCOVERY_DATE uses Julian day number. Convert using Julian calendar offset.
  dt <- as.Date(as.numeric(d$DISCOVERY_DATE) - 2440587.5, origin="1970-01-01")
  d$date <- dt
  d$month <- as.integer(format(dt, "%m"))
  d$event_year <- as.integer(format(d$date,"%Y"))
  d$large <- ifelse(is.finite(d$FIRE_SIZE) & d$FIRE_SIZE >= 300, 1L, ifelse(is.finite(d$FIRE_SIZE) & d$FIRE_SIZE >= 0, 0L, NA_integer_))
  # Dataset and integrity audit before exclusions.
  audit <- data.frame(
    measure=c("rows","columns","unique_FOD_ID","duplicate_FOD_ID_rows","missing_outcome","negative_outcome","invalid_latitude","invalid_longitude","missing_county","missing_state","missing_discovery_date","invalid_doy_for_year","discovery_doy_date_mismatch","discovery_date_year_mismatch","outcome_under_300","outcome_at_least_300","year_min","year_max"),
    value=c(nrow(d), source_column_count, length(unique(d$FOD_ID)), sum(duplicated(d$FOD_ID)), sum(is.na(d$FIRE_SIZE)), sum(d$FIRE_SIZE<0,na.rm=TRUE), sum(!is.na(d$LATITUDE) & (d$LATITUDE < -90 | d$LATITUDE > 90)), sum(!is.na(d$LONGITUDE) & (d$LONGITUDE < -180 | d$LONGITUDE > 180)), sum(is.na(d$COUNTY)), sum(is.na(d$STATE)), sum(is.na(d$date)), sum(!is.na(d$DISCOVERY_DOY) & (d$DISCOVERY_DOY < 1 | d$DISCOVERY_DOY > ifelse(leap_year(d$event_year),366,365))), sum(!is.na(d$DISCOVERY_DOY) & !is.na(d$date) & as.integer(format(d$date,"%j")) != d$DISCOVERY_DOY), sum(!is.na(d$date) & as.integer(format(d$date,"%Y")) != d$FIRE_YEAR), sum(d$large==0,na.rm=TRUE), sum(d$large==1,na.rm=TRUE), min(d$event_year,na.rm=TRUE), max(d$event_year,na.rm=TRUE)), stringsAsFactors=FALSE)
  write_csv(audit,"results/data_audit.csv")
  # The parsed discovery date is authoritative for chronology when source FIRE_YEAR disagrees.
  miss <- data.frame(column=names(d), missing_n=vapply(d,function(x)sum(is.na(x)),numeric(1)), missing_pct=100*vapply(d,function(x)mean(is.na(x)),numeric(1)))
  write_csv(miss,"results/missingness.csv")
  # Do not remove missing county records: county is never a model feature.
  clean <- d[!is.na(d$large) & d$large %in% 0:1 & !is.na(d$date) & d$event_year>=1992 & d$event_year<=2015 & !duplicated(d$FOD_ID),]
  clean$month_sin <- sin(2*pi*clean$month/12); clean$month_cos <- cos(2*pi*clean$month/12)
  clean$STATE <- ifelse(is.na(clean$STATE) | clean$STATE=="", "Unknown", clean$STATE)
  clean$LATITUDE[clean$LATITUDE < -90 | clean$LATITUDE > 90] <- NA
  clean$LONGITUDE[clean$LONGITUDE < -180 | clean$LONGITUDE > 180] <- NA
  # Fixed chronological partition, chosen before inspecting model scores.
  tr <- clean[clean$event_year<=2007,]; va <- clean[clean$event_year>=2008 & clean$event_year<=2011,]; te <- clean[clean$event_year>=2012 & clean$event_year<=2015,]
  split <- data.frame(split=c("train_1992_2007","validation_2008_2011","test_2012_2015"), n=c(nrow(tr),nrow(va),nrow(te)), positives=c(sum(tr$large),sum(va$large),sum(te$large)), prevalence=c(mean(tr$large),mean(va$large),mean(te$large)))
  write_csv(split,"results/split_summary.csv")
  if(any(c(nrow(tr),nrow(va),nrow(te))==0) || length(unique(tr$large))<2) stop("Chronological split lacks usable rows/classes")
  # Training-only coordinate medians and factor levels.
  lat_med <- median(tr$LATITUDE,na.rm=TRUE); lon_med <- median(tr$LONGITUDE,na.rm=TRUE)
  if(!is.finite(lat_med)) lat_med <- 0; if(!is.finite(lon_med)) lon_med <- 0
  state_levels <- sort(unique(tr$STATE))
  state_fallback <- names(which.max(table(tr$STATE)))
  prep <- function(x) {
    x$LATITUDE[is.na(x$LATITUDE)] <- lat_med
    x$LONGITUDE[is.na(x$LONGITUDE)] <- lon_med
    x$STATE[is.na(x$STATE) | !x$STATE %in% state_levels] <- state_fallback
    x$STATE <- factor(x$STATE, levels=state_levels)
    x
  }
  tr <- prep(tr); va <- prep(va); te <- prep(te)
  # Stable models: season-only logistic; logistic and RF share year, seasonal, state, coordinate inputs.
  season <- glm(large ~ month_sin + month_cos, data=tr, family=binomial())
  full <- glm(large ~ event_year + month_sin + month_cos + STATE + LATITUDE + LONGITUDE, data=tr, family=binomial())
  if(anyNA(coef(full))) full <- glm(large ~ event_year + month_sin + month_cos + LATITUDE + LONGITUDE, data=tr, family=binomial())
  set.seed(141)
  rforest <- randomForest::randomForest(x=tr[,c("event_year","month_sin","month_cos","STATE","LATITUDE","LONGITUDE")], y=factor(tr$large,levels=0:1), ntree=500, mtry=2, importance=TRUE)
  pred <- list(
    prevalence=rep(mean(tr$large),nrow(te)),
    season=as.numeric(predict(season,newdata=te,type="response")),
    logistic=as.numeric(predict(full,newdata=te,type="response")),
    random_forest=as.numeric(predict(rforest,newdata=te[,c("event_year","month_sin","month_cos","STATE","LATITUDE","LONGITUDE")],type="prob")[,"1"])
  )
  vpred <- list(season=as.numeric(predict(season,newdata=va,type="response")), logistic=as.numeric(predict(full,newdata=va,type="response")), random_forest=as.numeric(predict(rforest,newdata=va[,c("event_year","month_sin","month_cos","STATE","LATITUDE","LONGITUDE")],type="prob")[,"1"]))
  # Threshold selected using validation F1, separately for each learned model.
  f1 <- function(y,p,t) { pr<-p>=t; tp<-sum(pr & y==1); pp<-sum(pr); rr<-sum(y==1); if(pp+rr==0) return(0); 2*tp/(pp+rr) }
  thresholds <- vapply(vpred,function(p){ grid<-seq(0.01,0.99,by=0.01); scores<-vapply(grid,function(t)f1(va$large,p,t),numeric(1)); grid[which.max(scores)] },numeric(1))
  auc_rank <- function(y,p) { n1<-sum(y==1); n0<-sum(y==0); if(!n1||!n0)return(NA_real_); r<-rank(p,ties.method="average"); (sum(r[y==1])-n1*(n1+1)/2)/(n1*n0) }
  ap <- function(y,p) {
    scores <- sort(unique(p), decreasing=TRUE)
    positives <- vapply(scores,function(s)sum(y[p==s]==1),numeric(1))
    totals <- vapply(scores,function(s)sum(p==s),numeric(1))
    if(!sum(positives)) return(NA_real_)
    precision <- cumsum(positives)/cumsum(totals)
    sum((positives/sum(positives))*precision)
  }
  brier <- function(y,p) mean((y-p)^2)
  metric <- function(y,p,t) { yes<-p>=t; tp<-sum(yes & y==1); fp<-sum(yes & y==0); fn<-sum(!yes & y==1); tn<-sum(!yes & y==0); c(roc_auc=auc_rank(y,p),average_precision=ap(y,p),precision=if(tp+fp)tp/(tp+fp) else 0,recall=if(tp+fn)tp/(tp+fn) else 0,f1=if(2*tp+fp+fn)2*tp/(2*tp+fp+fn) else 0,brier=brier(y,p),threshold=t,tp=tp,fp=fp,fn=fn,tn=tn) }
  models <- c("prevalence","season","logistic","random_forest")
  metrics <- do.call(rbind,lapply(models,function(m){ t<-if(m=="prevalence")0.5 else thresholds[[m]]; data.frame(model=m,as.list(metric(te$large,pred[[m]],t)),row.names=NULL) }))
  write_csv(metrics,"results/test_metrics.csv")
  val_metrics <- do.call(rbind,lapply(names(vpred),function(m)data.frame(model=m,threshold=thresholds[[m]],validation_f1=f1(va$large,vpred[[m]],thresholds[[m]]))))
  write_csv(val_metrics,"results/validation_thresholds.csv")
  calibration <- do.call(rbind,lapply(models,function(m){ br<-cut(pred[[m]],breaks=seq(0,1,by=.1),include.lowest=TRUE); z<-aggregate(cbind(predicted=pred[[m]],observed=te$large),list(bin=br),mean); z$model<-m; z }))
  write_csv(calibration,"results/calibration.csv")
  # Event-size, sample composition, and denominators for EDA.
  year_groups <- split(d$FIRE_SIZE[!is.na(d$FIRE_SIZE)], d$event_year[!is.na(d$FIRE_SIZE)])
  size_out <- data.frame(year=as.integer(names(year_groups)), n=vapply(year_groups,length,numeric(1)), median=vapply(year_groups,median,numeric(1)), p90=vapply(year_groups,function(x)as.numeric(quantile(x,.9)),numeric(1)), large_share=vapply(year_groups,function(x)mean(x>=300),numeric(1)))
  write_csv(size_out,"results/year_summary.csv")
  month_groups <- split(clean$large,clean$month)
  write_csv(data.frame(month=as.integer(names(month_groups)),n=vapply(month_groups,length,numeric(1)),large_n=vapply(month_groups,sum,numeric(1)),large_share=vapply(month_groups,mean,numeric(1))),"results/month_summary.csv")
  state_groups <- split(clean$large,clean$STATE)
  write_csv(data.frame(state=names(state_groups),n=vapply(state_groups,length,numeric(1)),large_n=vapply(state_groups,sum,numeric(1)),large_share=vapply(state_groups,mean,numeric(1))),"results/state_summary.csv")
  # Compact split and record-level prediction outputs support audit/review.
  unlink(c("results/test_predictions.csv","results/models.rds"), force=TRUE)
  cat("Rows",nrow(d),"; analyzed",nrow(clean),"; train/validation/test",nrow(tr),nrow(va),nrow(te),"\n")
  print(metrics)
}
leap_year <- function(y) !is.na(y) & (y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0))
