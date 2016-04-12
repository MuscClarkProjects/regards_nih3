library(pROC)

auc_from_accuracy <- function(acc, ncases, ncontrols, incorrectcontrib) {
  
  n = ncases + ncontrols
  ncorrect = acc * n
  
  nmin_sens = min_sens(ncorrect, ncases, ncontrols, incorrectcontrib)
  
  nmin_spec = min_spec(ncorrect, ncases, ncontrols, incorrectcontrib)
  
  list(sens=nmin_sens, spec=nmin_spec)
}


min_sens <- function(ncorrect, ncases, ncontrols, incorrectcontrib) {
  
  n = ncases + ncontrols
  nincorrect = n - ncorrect
  
  nincorrect_cases = min(nincorrect * incorrectcontrib, ncases)
  nincorrect_controls = min(nincorrect - nincorrect_cases, ncontrols)
  
  ncorrect_cases = ncases - nincorrect_cases
  ncorrect_controls = ncontrols - nincorrect_controls
  
  
  print(paste('ncorrect_controls:', ncorrect_controls))
  print(paste('ncorrect_cases:', ncorrect_cases))
  
  auc = ncorrect_cases/ncases #square wave
  
  power.roc.test(auc, power=0.8)$ncases * 2
}


min_spec <- function(ncorrect, ncases, ncontrols, incorrectcontrib) {
  min_sens(ncorrect, ncontrols, ncases, incorrectcontrib)
}