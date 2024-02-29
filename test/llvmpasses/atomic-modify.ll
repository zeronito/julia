; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt -enable-new-pm=1 --opaque-pointers=0 --load-pass-plugin=libjulia-codegen%shlibext -passes='ExpandAtomicModify' -S %s | FileCheck %s --check-prefixes=CHECK,TYPED

; RUN: opt -enable-new-pm=1 --opaque-pointers=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='ExpandAtomicModify' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

declare i8 @julia.atomicmodify.i8(i8 (i8, i8)*, i8)
declare double @julia.atomicmodify.f64(double (double, double)*, double)
declare double @llvm.maxnum.f64(double %Val0, double %Val1)

define i8 @add.i8(i8 %a, i8 %b) {
    %c = add i8 %b, %a
    ret i8 %c
}

define i8 @sub.i8(i8 %a, i8 %b) {
    %c = sub i8 %a, %b
    ret i8 %c
}

define i8 @subx.i8(i8 %a, i8 %b) {
    %c = sub i8 %b, %a
    ret i8 %c
}

define i8 @xchg.i8(i8 %a, i8 %b) {
    ret i8 %b
}

define double @fadd.f64(double %a, double %b) {
    %c = fadd double %b, %a
    ret double %c
}

define double @fmax.f64(double %a, double %b) {
    %c = call double @llvm.maxnum.f64(double %b, double %a)
    ret double %c
}

define internal i8 @0(i8 %a, i8 %b) unnamed_addr {
    %c = call i8 @add.i8(i8 %a, i8 %b);
    ret i8 %c
}

define internal i8 @1(i8 %a, i8 %b) unnamed_addr {
    %c = call i8 @0(i8 %a, i8 %b);
    ret i8 %c
}

define i8 @mod_i8_add(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_add
; TYPED: %oldval = atomicrmw add i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @add.i8, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_add_new(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_add
; TYPED: %oldval = atomicrmw add i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK-NEXT: %newval = add i8 %oldval, %b
; CHECK-NEXT: ret i8 %newval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @add.i8, i8 %oldval)
  ret i8 %newval
}

define i8 @mod_i8_sub(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_sub
; TYPED: %oldval = atomicrmw sub i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw sub ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @sub.i8, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_subx(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_subx
; CHECK: call i8 @subx.i8(i8 %loaded, i8 %b)
; CHECK: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @subx.i8, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_subx_new(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_subx_new
; CHECK: %newval = call i8 @subx.i8(i8 %loaded, i8 %b)
; CHECK: ret i8 %newval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @subx.i8, i8 %oldval)
  ret i8 %newval
}

define i8 @mod_i8_xchg(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_xchg
; TYPED: %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw xchg ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @xchg.i8, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_xchg_new(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_xchg_new
; TYPED: %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw xchg ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %b
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @xchg.i8, i8 %oldval)
  ret i8 %newval
}

define double @mod_i8_fadd(double* %a, double %b) {
; CHECK-LABEL: @mod_i8_fadd
; TYPED: %oldval = atomicrmw fadd double* %a, double %b release, align 1
; OPAQUE: %oldval = atomicrmw fadd ptr %a, double %b release, align 1
; CHECK-NEXT: ret double %oldval
top:
  %oldval = atomicrmw xchg double* %a, double %b release, align 1
  %newval = call double @julia.atomicmodify.f64(double (double, double)* @fadd.f64, double %oldval)
  ret double %oldval
}

define double @mod_i8_fmax(double* %a, double %b) {
; CHECK-LABEL: @mod_i8_fmax
; TYPED: %oldval = atomicrmw fmax double* %a, double %b release, align 1
; OPAQUE: %oldval = atomicrmw fmax ptr %a, double %b release, align 1
; CHECK-NEXT: ret double %oldval
top:
  %oldval = atomicrmw xchg double* %a, double %b release, align 1
  %newval = call double @julia.atomicmodify.f64(double (double, double)* @fmax.f64, double %oldval)
  ret double %oldval
}

define i8 @mod_i8_indirect0(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect0
; TYPED: %oldval = atomicrmw add i8* %a, i8 %b release, align 1
; OPAQUE: %oldval = atomicrmw add ptr %a, i8 %b release, align 1
; CHECK-NEXT: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @0, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_indirect1(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect1
; CHECK: %newval = call i8 @0(i8 %loaded, i8 %b)
; CHECK: ret i8 %oldval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @1, i8 %oldval)
  ret i8 %oldval
}

define i8 @mod_i8_indirect1_new(i8* %a, i8 %b) {
; CHECK-LABEL: @mod_i8_indirect1
; CHECK: %newval = call i8 @0(i8 %loaded, i8 %b)
; CHECK: ret i8 %newval
top:
  %oldval = atomicrmw xchg i8* %a, i8 %b release, align 1
  %newval = call i8 @julia.atomicmodify.i8(i8 (i8, i8)* @1, i8 %oldval)
  ret i8 %newval
}
