// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// dig_vec
NumericVector dig_vec(NumericVector u, NumericVector v, NumericVector theta, NumericVector alpha);
RcppExport SEXP _igcop_dig_vec(SEXP uSEXP, SEXP vSEXP, SEXP thetaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(dig_vec(u, v, theta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_gen_vec
NumericVector igl_gen_vec(NumericVector x, NumericVector alpha);
RcppExport SEXP _igcop_igl_gen_vec(SEXP xSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_gen_vec(x, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_gen_D_vec
NumericVector igl_gen_D_vec(NumericVector x, NumericVector alpha);
RcppExport SEXP _igcop_igl_gen_D_vec(SEXP xSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_gen_D_vec(x, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_gen_inv_vec
NumericVector igl_gen_inv_vec(NumericVector p, NumericVector alpha);
RcppExport SEXP _igcop_igl_gen_inv_vec(SEXP pSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_gen_inv_vec(p, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_kappa_vec
NumericVector igl_kappa_vec(NumericVector x, NumericVector alpha);
RcppExport SEXP _igcop_igl_kappa_vec(SEXP xSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_kappa_vec(x, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_kappa_D_vec
NumericVector igl_kappa_D_vec(NumericVector x, NumericVector alpha);
RcppExport SEXP _igcop_igl_kappa_D_vec(SEXP xSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_kappa_D_vec(x, alpha));
    return rcpp_result_gen;
END_RCPP
}
// igl_kappa_inv_vec
NumericVector igl_kappa_inv_vec(NumericVector p, NumericVector alpha);
RcppExport SEXP _igcop_igl_kappa_inv_vec(SEXP pSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(igl_kappa_inv_vec(p, alpha));
    return rcpp_result_gen;
END_RCPP
}
// interp_gen_vec
NumericVector interp_gen_vec(NumericVector x, NumericVector eta, NumericVector alpha);
RcppExport SEXP _igcop_interp_gen_vec(SEXP xSEXP, SEXP etaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(interp_gen_vec(x, eta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// interp_gen_inv_vec
NumericVector interp_gen_inv_vec(NumericVector p, NumericVector eta, NumericVector alpha);
RcppExport SEXP _igcop_interp_gen_inv_vec(SEXP pSEXP, SEXP etaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(interp_gen_inv_vec(p, eta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// interp_kappa_vec
NumericVector interp_kappa_vec(NumericVector x, NumericVector eta, NumericVector alpha);
RcppExport SEXP _igcop_interp_kappa_vec(SEXP xSEXP, SEXP etaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(interp_kappa_vec(x, eta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// interp_kappa_inv_vec
NumericVector interp_kappa_inv_vec(NumericVector p, NumericVector eta, NumericVector alpha);
RcppExport SEXP _igcop_interp_kappa_inv_vec(SEXP pSEXP, SEXP etaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(interp_kappa_inv_vec(p, eta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// pcondig12_vec
NumericVector pcondig12_vec(NumericVector u, NumericVector v, NumericVector theta, NumericVector alpha);
RcppExport SEXP _igcop_pcondig12_vec(SEXP uSEXP, SEXP vSEXP, SEXP thetaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(pcondig12_vec(u, v, theta, alpha));
    return rcpp_result_gen;
END_RCPP
}
// qcondig12_vec
NumericVector qcondig12_vec(NumericVector p, NumericVector v, NumericVector theta, NumericVector alpha);
RcppExport SEXP _igcop_qcondig12_vec(SEXP pSEXP, SEXP vSEXP, SEXP thetaSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(qcondig12_vec(p, v, theta, alpha));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_igcop_dig_vec", (DL_FUNC) &_igcop_dig_vec, 4},
    {"_igcop_igl_gen_vec", (DL_FUNC) &_igcop_igl_gen_vec, 2},
    {"_igcop_igl_gen_D_vec", (DL_FUNC) &_igcop_igl_gen_D_vec, 2},
    {"_igcop_igl_gen_inv_vec", (DL_FUNC) &_igcop_igl_gen_inv_vec, 2},
    {"_igcop_igl_kappa_vec", (DL_FUNC) &_igcop_igl_kappa_vec, 2},
    {"_igcop_igl_kappa_D_vec", (DL_FUNC) &_igcop_igl_kappa_D_vec, 2},
    {"_igcop_igl_kappa_inv_vec", (DL_FUNC) &_igcop_igl_kappa_inv_vec, 2},
    {"_igcop_interp_gen_vec", (DL_FUNC) &_igcop_interp_gen_vec, 3},
    {"_igcop_interp_gen_inv_vec", (DL_FUNC) &_igcop_interp_gen_inv_vec, 3},
    {"_igcop_interp_kappa_vec", (DL_FUNC) &_igcop_interp_kappa_vec, 3},
    {"_igcop_interp_kappa_inv_vec", (DL_FUNC) &_igcop_interp_kappa_inv_vec, 3},
    {"_igcop_pcondig12_vec", (DL_FUNC) &_igcop_pcondig12_vec, 4},
    {"_igcop_qcondig12_vec", (DL_FUNC) &_igcop_qcondig12_vec, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_igcop(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
