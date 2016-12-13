#include <R.h>
#include <Rinternals.h>

double  median_inplace(double *x, int length){
  int half;
  double med;
  double *buffer = x;
  half = (length + 1)/2;

  rPsort(buffer, length, half-1);
  med = buffer[half-1];
  if (length % 2 == 0){
    rPsort(buffer, length, half);
    med = (med + buffer[half])/2.0;
  }

  return med;
}

SEXP group_to_median_sorted_(SEXP x, SEXP f) {

  /* x must be sorted by f */
  int nlevs = nlevels(f);

  if (TYPEOF(x) != REALSXP)
    error("First argument must be a numeric.");
  if (!isFactor(f))
    error("First argument must be a factor.");
  
  SEXP r = PROTECT(allocVector(REALSXP, nlevs));

  int *fc = INTEGER(f);
  double *xc = REAL(x);
  double *rc = REAL(r);
  int lv = fc[0];
  int cnt = 0;
  double obs[4];

  int i, lv_;
  for (i=0; i<LENGTH(x); ++i) {
    lv_ = fc[i];
    if (lv != lv_) {
      rc[lv-1] = median_inplace(obs, cnt);
      lv = lv_; cnt = 0;
    }

    obs[cnt++] = xc[i];
  }

  /* clean up */
  if (cnt >= 1)
    rc[lv-1] = median_inplace(obs, cnt);

  setAttrib(r, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
  UNPROTECT(1);
  return r;
}

