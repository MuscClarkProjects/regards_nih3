auc_from_accuracy <- function(acc, ncases, ncontrols) {
  n = ncases + ncontrols
  ncorrect = acc * n
  
  min_sens_n = min_sens(ncorrect, ncases, ncontrols)
  
  min_spec_n = min_spec(ncorrect, ncases, ncontrols)
  
  list(sens=min_sens_n, spec=min_spec_n)
}

min_sens <- function(ncorrect, ncases, ncontrols) {
  
  ncorrect_controls = min(ncontrols, ncorrect)
  ncorrect_cases = max(ncorrect-ncontrols, 0)
  
  auc = ncorrect_cases/ncases #square wave
  
  power.roc.test(auc, power=0.8)$ncases * 2
}


min_spec <- function(ncorrect, ncases, ncontrols) {
  min_sens(ncorrect, ncontrols, ncases)
}