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

SEXP group_to_median_(SEXP x, SEXP f) {
  int nlevs = nlevels(f);

  if (TYPEOF(x) != REALSXP)
    error("First argument must be a numeric.");
  if (!isFactor(f))
    error("First argument must be a factor.");
  
  SEXP r = PROTECT(allocVector(REALSXP, nlevs));

  int *fc = INTEGER(f);
  double *xc = REAL(x);
  double *rc = REAL(r);
  int lv1 = -1;
  int lv2 = -1;
  int cnt1 = 4;
  int cnt2 = 4;
  double obs1[4], obs2[4];

  int i, lv;
  for (i=0; i<LENGTH(x); ++i) {
    lv = fc[i];
    if (lv != lv1 && lv != lv2) {
      if (cnt1 >= 3) {          /* reset allele 1 */
        if (lv1 >= 0) {
          rc[lv1-1] = median_inplace(obs1, cnt1);
          /* Rprintf("lvl: %d\tcnt: %d\tmedian: %1.2f\n", lv1, cnt1, rc[lv1]); */
        }
        lv1 = lv; cnt1 = 0;
      } else if (cnt2 >= 3) {   /* reset allele 2 */
        if (lv2 >= 0) {
          rc[lv2-1] = median_inplace(obs2, cnt2);
          /* Rprintf("lvl: %d\tcnt: %d\tmedian: %1.2f\n", lv2, cnt2, rc[lv2]); */
        }
        lv2 = lv; cnt2 = 0;
      }
    }

    if (lv == lv1) {
      obs1[cnt1] = xc[i];
      cnt1++;
    } else if (lv == lv2) {
      obs2[cnt2] = xc[i];
      cnt2++;
    }
  }

  /* clean up */
  if (cnt1 >= 1 && lv1 >= 0)
    rc[lv1-1] = median_inplace(obs1, cnt1);

  if (cnt2 >= 1 && lv2 >= 0)
    rc[lv2-1] = median_inplace(obs2, cnt2);

  setAttrib(r, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
  UNPROTECT(1);
  return r;
}

