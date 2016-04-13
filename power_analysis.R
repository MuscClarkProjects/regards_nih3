library(pROC)

worse_case_from_accuracy <- function(acc, ncases, ncontrols, incorrectcontrib) {
  
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


get_sample_size <- function(min_sens, min_spec) {
  auc = min_sens * min_spec
  power.roc.test(auc, power=0.8)$ncases * 2
}


counts_for_accuracy_lizardui_et_al <- function(){
  accs = c(75.2, 76.7, 86.1, 90.7, 97.7, 92.2, 94.6, 94.6)
  accs_ratio = accs * .01
  features = c('ssf', 'ssf+fd1', 'ssf+fd2', 'ef', 'ef+te',
               'ssf+ef', 'ssf+fd2+ef', 'ssf+fd2+ef+te')
  ncases = 20
  ncontrols = 20
  
  get_sens_spec = function(incorrectcontrib) {
    from_acc = function(acc)( 
      worse_case_from_accuracy(acc, ncases, ncontrols, incorrectcontrib))
    
    ret = sapply(accs_ratio, from_acc)
    sens = sapply(ret["sens",], identity)
    spec = sapply(ret["spec",], identity)
    list(sens=sens, spec=spec)
  }
  
  very_worse = get_sens_spec(1.)
  half_bad = get_sens_spec(.5)
  
  data.frame(features, accs, 
             very_worse$sens, very_worse$spec,
             half_bad$sens, half_bad$spec)
}
