// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_Xv_RCPPEXPORTS_H_GEN_
#define RCPP_Xv_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace Xv {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("Xv", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("Xv", "Xv_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in Xv");
            }
        }
    }

    inline SEXP Xv_dgCMatrix_numeric(S4 x, NumericVector y) {
        typedef SEXP(*Ptr_Xv_dgCMatrix_numeric)(SEXP,SEXP);
        static Ptr_Xv_dgCMatrix_numeric p_Xv_dgCMatrix_numeric = NULL;
        if (p_Xv_dgCMatrix_numeric == NULL) {
            validateSignature("SEXP(*Xv_dgCMatrix_numeric)(S4,NumericVector)");
            p_Xv_dgCMatrix_numeric = (Ptr_Xv_dgCMatrix_numeric)R_GetCCallable("Xv", "Xv_Xv_dgCMatrix_numeric");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_Xv_dgCMatrix_numeric(Rcpp::wrap(x), Rcpp::wrap(y));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP Xv_dgTMatrix_numeric(S4 x, NumericVector y) {
        typedef SEXP(*Ptr_Xv_dgTMatrix_numeric)(SEXP,SEXP);
        static Ptr_Xv_dgTMatrix_numeric p_Xv_dgTMatrix_numeric = NULL;
        if (p_Xv_dgTMatrix_numeric == NULL) {
            validateSignature("SEXP(*Xv_dgTMatrix_numeric)(S4,NumericVector)");
            p_Xv_dgTMatrix_numeric = (Ptr_Xv_dgTMatrix_numeric)R_GetCCallable("Xv", "Xv_Xv_dgTMatrix_numeric");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_Xv_dgTMatrix_numeric(Rcpp::wrap(x), Rcpp::wrap(y));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP vX_numeric_dgCMatrix(NumericVector x, S4 y) {
        typedef SEXP(*Ptr_vX_numeric_dgCMatrix)(SEXP,SEXP);
        static Ptr_vX_numeric_dgCMatrix p_vX_numeric_dgCMatrix = NULL;
        if (p_vX_numeric_dgCMatrix == NULL) {
            validateSignature("SEXP(*vX_numeric_dgCMatrix)(NumericVector,S4)");
            p_vX_numeric_dgCMatrix = (Ptr_vX_numeric_dgCMatrix)R_GetCCallable("Xv", "Xv_vX_numeric_dgCMatrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_vX_numeric_dgCMatrix(Rcpp::wrap(x), Rcpp::wrap(y));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP vX_numeric_dgTMatrix(NumericVector x, S4 y) {
        typedef SEXP(*Ptr_vX_numeric_dgTMatrix)(SEXP,SEXP);
        static Ptr_vX_numeric_dgTMatrix p_vX_numeric_dgTMatrix = NULL;
        if (p_vX_numeric_dgTMatrix == NULL) {
            validateSignature("SEXP(*vX_numeric_dgTMatrix)(NumericVector,S4)");
            p_vX_numeric_dgTMatrix = (Ptr_vX_numeric_dgTMatrix)R_GetCCallable("Xv", "Xv_vX_numeric_dgTMatrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_vX_numeric_dgTMatrix(Rcpp::wrap(x), Rcpp::wrap(y));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

}

#endif // RCPP_Xv_RCPPEXPORTS_H_GEN_