# Regimen Test Case Catalogue

Reference document cataloguing real-world oncology regimens as potential test cases for `openqalyregimen`. Each entry includes exact dosing parameters and a verdict on whether the package can model it.

**Legend:**

- **YES** -- fully modelable with current package features
- **PARTIAL** -- modelable with workarounds or only certain phases can be modeled
- **NO** -- cannot be modeled (feature not supported)

---

## 1. IV Monotherapy

### 1.1 Pembrolizumab (Keytruda)

| Parameter | Value |
|-----------|-------|
| Indication | NSCLC, melanoma, urothelial, others |
| Dose | 200 mg Q3W or 400 mg Q6W |
| Dose basis | Flat |
| Cycle length | 21 days (Q3W) or 42 days (Q6W) |
| Admin days | Day 1 |
| Max duration | 35 cycles Q3W (~2 years) |
| Vial sizes | 100 mg |
| Vial cost (UK NHS) | GBP 2,630 per 100 mg vial |

**Verdict: YES** -- flat dose, single admin day, single vial size.

### 1.2 Nivolumab (Opdivo)

| Parameter | Value |
|-----------|-------|
| Indication | Melanoma, NSCLC, RCC, oesophageal |
| Dose | 240 mg Q2W or 480 mg Q4W (flat); historically 3 mg/kg Q2W |
| Dose basis | Flat (current) or weight-based (historical) |
| Cycle length | 14 days (Q2W) or 28 days (Q4W) |
| Admin days | Day 1 |
| Max duration | Up to 2 years |
| Vial sizes | 40 mg, 100 mg, 120 mg, 240 mg (all 10 mg/mL) |
| Vial costs (UK NHS) | GBP 439 (40 mg), GBP 1,097 (100 mg), GBP 1,317 (120 mg), GBP 2,633 (240 mg) |

**Verdict: YES** -- both flat and weight-based versions are directly supported. Multi-vial optimization works.

### 1.3 Trastuzumab (Herceptin) -- IV

| Parameter | Value |
|-----------|-------|
| Indication | HER2+ breast cancer (adjuvant/metastatic), HER2+ gastric |
| Dose | Loading: 8 mg/kg; Maintenance: 6 mg/kg Q3W |
| Dose basis | Weight-based (mg/kg) |
| Cycle length | 21 days (Q3W) |
| Admin days | Day 1 |
| Max duration | 18 cycles (~1 year) adjuvant; until progression metastatic |
| Vial sizes | 150 mg, 420 mg (powder) |
| Vial cost (UK NHS) | GBP 407.40 per 150 mg (originator) |

**Verdict: PARTIAL** -- maintenance dose (6 mg/kg) is fully modelable. Loading dose (8 mg/kg) cannot be modeled within the same regimen. Workaround: two separate regimen objects (1 cycle at 8 mg/kg + remaining at 6 mg/kg).

### 1.4 Rituximab (MabThera/Rituxan)

| Parameter | Value |
|-----------|-------|
| Indication | Follicular lymphoma, DLBCL, CLL |
| Dose | 375 mg/m2 |
| Dose basis | BSA (mg/m2) |
| Cycle length | 7 days (weekly x4 monotherapy), 21 days (R-CHOP), 56 days (maintenance) |
| Admin days | Day 1 |
| Max duration | 4-8 cycles induction; up to 2 years maintenance |
| Vial sizes | 100 mg, 500 mg (both 10 mg/mL) |
| Vial costs (UK NHS) | GBP 174.63 (100 mg), GBP 873.15 (500 mg) |

**Verdict: YES** -- BSA-based, single admin day, multi-vial optimization. Different phases (induction vs maintenance) require separate regimen objects.

### 1.5 Bevacizumab (Avastin)

| Parameter | Value |
|-----------|-------|
| Indication | mCRC, NSCLC, ovarian, RCC, cervical, glioblastoma |
| Dose | 5, 7.5, 10, or 15 mg/kg (indication-dependent) |
| Dose basis | Weight-based (mg/kg) |
| Cycle length | 14 days (Q2W) or 21 days (Q3W) |
| Admin days | Day 1 |
| Max duration | Until progression; up to 22 cycles (ovarian) |
| Vial sizes | 100 mg, 400 mg (both 25 mg/mL) |
| Vial costs (UK NHS) | GBP 242.66 (100 mg), GBP 924.40 (400 mg) |

**Verdict: YES**

### 1.6 Ipilimumab (Yervoy)

| Parameter | Value |
|-----------|-------|
| Indication | Melanoma (mono and combo); RCC, NSCLC, mesothelioma |
| Dose | 3 mg/kg (metastatic melanoma); 10 mg/kg (adjuvant) |
| Dose basis | Weight-based (mg/kg) |
| Cycle length | 21 days (Q3W) |
| Admin days | Day 1 |
| Max duration | 4 doses (induction metastatic); adjuvant: 4 Q3W then Q12W up to 3 years |
| Vial sizes | 50 mg, 200 mg (both 5 mg/mL) |
| Vial costs (UK NHS) | GBP 3,750 (50 mg), GBP 15,000 (200 mg) |

**Verdict: YES** for 4-dose induction (`max_med_cycles = 4`). Adjuvant schedule change from Q3W to Q12W requires two separate regimen objects.

### 1.7 Atezolizumab (Tecentriq)

| Parameter | Value |
|-----------|-------|
| Indication | NSCLC, SCLC, HCC, urothelial |
| Dose | 1200 mg flat dose |
| Dose basis | Flat |
| Cycle length | 21 days (Q3W) |
| Admin days | Day 1 |
| Max duration | Until progression |
| Vial sizes | 1200 mg (single-use) |
| Vial cost (UK NHS) | GBP 3,807.69 per 1200 mg vial |

**Verdict: YES** -- simplest possible case: flat dose, single vial matches dose exactly.

### 1.8 Durvalumab (Imfinzi)

| Parameter | Value |
|-----------|-------|
| Indication | Stage III NSCLC (PACIFIC), biliary tract, SCLC |
| Dose | 10 mg/kg Q2W or 1500 mg Q4W (flat) |
| Dose basis | Weight-based or flat |
| Cycle length | 14 days (Q2W) or 28 days (Q4W) |
| Admin days | Day 1 |
| Max duration | Up to 12 months |
| Vial sizes | 120 mg, 500 mg (both 50 mg/mL) |
| Vial costs (UK NHS) | GBP 592 (120 mg), GBP 2,466 (500 mg) |

**Verdict: YES**

### 1.9 Avelumab (Bavencio)

| Parameter | Value |
|-----------|-------|
| Indication | Urothelial maintenance, Merkel cell |
| Dose | 10 mg/kg or 800 mg Q2W (flat) |
| Dose basis | Weight-based or flat |
| Cycle length | 14 days (Q2W) |
| Admin days | Day 1 |
| Max duration | Until progression |
| Vial sizes | 200 mg (20 mg/mL) |
| Vial cost (UK NHS) | GBP 768 per 200 mg vial |

**Verdict: YES**

### 1.10 Cetuximab (Erbitux)

| Parameter | Value |
|-----------|-------|
| Indication | mCRC (RAS wild-type), head and neck SCC |
| Dose | Loading: 400 mg/m2; Maintenance: 250 mg/m2 weekly or 500 mg/m2 biweekly |
| Dose basis | BSA (mg/m2) |
| Cycle length | 7 days (weekly) or 14 days (biweekly) |
| Admin days | Day 1 |
| Vial sizes | 100 mg, 500 mg (both 5 mg/mL) |
| Vial costs (UK NHS) | GBP 178.10 (100 mg), GBP 890.50 (500 mg) |

**Verdict: PARTIAL** -- maintenance dose modelable. Loading dose (400 mg/m2) requires a separate regimen object.

### 1.11 Panitumumab (Vectibix)

| Parameter | Value |
|-----------|-------|
| Indication | mCRC (RAS wild-type) |
| Dose | 6 mg/kg |
| Dose basis | Weight-based (mg/kg) |
| Cycle length | 14 days (Q2W) |
| Admin days | Day 1 |
| Max duration | Until progression |
| Vial sizes | 100 mg, 400 mg (both 20 mg/mL) |
| Vial costs (UK NHS) | GBP 379.29 (100 mg), GBP 1,517.16 (400 mg) |

**Verdict: YES**

### 1.12 Trastuzumab Emtansine (Kadcyla / T-DM1)

| Parameter | Value |
|-----------|-------|
| Indication | HER2+ mBC; HER2+ early BC (adjuvant after residual disease) |
| Dose | 3.6 mg/kg |
| Dose basis | Weight-based (mg/kg) |
| Cycle length | 21 days (Q3W) |
| Admin days | Day 1 |
| Max duration | 14 cycles (adjuvant); until progression (metastatic) |
| Vial sizes | 100 mg, 160 mg (powder) |
| Vial costs (UK NHS) | GBP 1,641.01 (100 mg), GBP 2,625.62 (160 mg) |

**Verdict: YES**

---

## 2. Oral Monotherapy

### 2.1 Ibrutinib (Imbruvica)

| Parameter | Value |
|-----------|-------|
| Indication | CLL/SLL, MCL, WM |
| Dose | 420 mg once daily (CLL); 560 mg (MCL) |
| Frequency | Once daily, continuous |
| Cycle length | Continuous (model as 28-day cycles) |
| Admin days | 1:28 |
| Max duration | Until progression |
| Tablet strengths | 140 mg, 280 mg, 420 mg, 560 mg |
| Cost (UK BNF) | GBP 1,430.80 per 28-pack of 140 mg |

**Verdict: YES** -- `med_cycle_length = 28`, `admin_days = 1:28`, `max_med_cycles = Inf`.

### 2.2 Lenalidomide (Revlimid)

| Parameter | Value |
|-----------|-------|
| Indication | Multiple myeloma |
| Dose | 25 mg once daily |
| Frequency | Once daily, days 1-21 of 28-day cycle |
| Cycle length | 28 days |
| Admin days | 1:21 (7 days off) |
| Max duration | Until progression |
| Capsule strengths | 2.5, 5, 7.5, 10, 15, 20, 25 mg |
| Cost (UK BNF) | GBP 4,368 per 21-capsule pack (25 mg) |

**Verdict: YES** -- `admin_days = 1:21`, `med_cycle_length = 28`.

### 2.3 Capecitabine (Xeloda)

| Parameter | Value |
|-----------|-------|
| Indication | Colorectal, gastric, breast cancer |
| Dose | 1,250 mg/m2 BID (mono) or 1,000 mg/m2 BID (combo) |
| Dose basis | BSA (mg/m2) |
| Frequency | Twice daily (BID), days 1-14 |
| Cycle length | 21 days |
| Admin days | Days 1-14, BID |
| Max duration | 8 cycles (adjuvant); until progression (metastatic) |
| Tablet strengths | 150 mg, 500 mg |
| Costs (UK BNF) | GBP 40.02 (60 x 150 mg), GBP 265.55 (120 x 500 mg) |

**Verdict: YES** (with workaround for BID) -- use `admin_days = rep(1:14, each = 2)` for 28 admin events per cycle, or `n_admin_per_cycle = 28`.

### 2.4 Olaparib (Lynparza)

| Parameter | Value |
|-----------|-------|
| Indication | Ovarian maintenance, breast cancer (BRCA+) |
| Dose | 300 mg (2 x 150 mg) twice daily |
| Frequency | Twice daily, continuous |
| Cycle length | Continuous (model as 28-day) |
| Admin days | Continuous daily |
| Max duration | Until progression; 2 years (non-BRCA ovarian) |
| Tablet strengths | 100 mg, 150 mg |
| Cost (UK BNF) | GBP 2,317.50 per 56-tablet pack (14-day supply) |

**Verdict: YES**

### 2.5 Palbociclib (Ibrance)

| Parameter | Value |
|-----------|-------|
| Indication | HR+/HER2- metastatic breast cancer |
| Dose | 125 mg once daily |
| Frequency | Once daily, days 1-21 of 28-day cycle |
| Cycle length | 28 days |
| Admin days | 1:21 (7 days off) |
| Max duration | Until progression |
| Capsule strengths | 75 mg, 100 mg, 125 mg |
| Cost (UK BNF) | GBP 2,950 per 21-capsule pack |

**Verdict: YES** -- `admin_days = 1:21`, `med_cycle_length = 28`.

### 2.6 Osimertinib (Tagrisso)

| Parameter | Value |
|-----------|-------|
| Indication | EGFR+ NSCLC |
| Dose | 80 mg once daily |
| Frequency | Once daily, continuous |
| Cycle length | Continuous (model as 30-day) |
| Max duration | Until progression (metastatic); 3 years (adjuvant) |
| Tablet strengths | 40 mg, 80 mg |
| Cost (UK BNF) | GBP 5,770 per 30-tablet pack |

**Verdict: YES**

### 2.7 Sunitinib (Sutent)

| Parameter | Value |
|-----------|-------|
| Indication | Renal cell carcinoma |
| Dose | 50 mg once daily |
| Frequency | Once daily, 4 weeks on / 2 weeks off |
| Cycle length | 42 days |
| Admin days | 1:28 (14 days off) |
| Max duration | Until progression |
| Capsule strengths | 12.5 mg, 25 mg, 37.5 mg, 50 mg |
| Cost (UK BNF) | GBP 2,730.76 per 28 x 50 mg capsules |

**Verdict: YES** -- `admin_days = 1:28`, `med_cycle_length = 42`.

### 2.8 Enzalutamide (Xtandi)

| Parameter | Value |
|-----------|-------|
| Indication | Prostate cancer |
| Dose | 160 mg once daily (4 x 40 mg) |
| Frequency | Once daily, continuous |
| Cycle length | Continuous (model as 28-day) |
| Max duration | Until progression |
| Tablet strengths | 40 mg, 80 mg |
| Cost (UK BNF) | GBP 2,734.67 per 112-tab pack of 40 mg (28-day supply) |

**Verdict: YES**

### 2.9 Ribociclib (Kisqali)

| Parameter | Value |
|-----------|-------|
| Indication | HR+/HER2- breast cancer |
| Dose | 600 mg once daily (metastatic); 400 mg (adjuvant) |
| Frequency | Once daily, days 1-21 of 28-day cycle |
| Cycle length | 28 days |
| Admin days | 1:21 (7 days off) |
| Max duration | Until progression; 3 years (adjuvant) |
| Tablet strengths | 200 mg (only strength) |
| Cost (UK BNF) | GBP 2,950 per 63-tablet pack |

**Verdict: YES** -- `admin_days = 1:21`, `med_cycle_length = 28`.

### 2.10 Venetoclax (Venclyxto)

| Parameter | Value |
|-----------|-------|
| Indication | CLL |
| Dose | 400 mg once daily (after 5-week ramp-up) |
| Ramp-up | Wk 1: 20 mg; Wk 2: 50 mg; Wk 3: 100 mg; Wk 4: 200 mg; Wk 5+: 400 mg |
| Frequency | Once daily, continuous |
| Cycle length | Continuous |
| Max duration | 12 months (with obinutuzumab); 24 months (with rituximab) |
| Tablet strengths | 10 mg, 50 mg, 100 mg |
| Costs (UK BNF) | GBP 4,789.47 per 112 x 100 mg; GBP 299.34 per 7 x 100 mg |

**Verdict: PARTIAL** -- 400 mg maintenance phase can be modeled. 5-week dose escalation ramp-up cannot be modeled (dose escalation not supported). Would require 5 separate regimen objects for the ramp-up period.

---

## 3. IV Combinations

### 3.1 R-CHOP-21 (DLBCL)

| Drug | Dose | Basis | Days | Route |
|------|------|-------|------|-------|
| Rituximab | 375 mg/m2 | BSA | Day 1 | IV |
| Cyclophosphamide | 750 mg/m2 | BSA | Day 1 | IV |
| Doxorubicin | 50 mg/m2 | BSA | Day 1 | IV |
| Vincristine | 1.4 mg/m2 (max 2 mg) | BSA (capped) | Day 1 | IV |
| Prednisone | 100 mg | Flat | Days 1-5 | Oral |

- **Cycle length:** 21 days, 6 cycles
- **Note:** Technically mixed-route due to prednisone.

**Verdict: PARTIAL** -- IV drugs on Day 1 can all be combined via `calculate_combination_costs()`. Vincristine 2 mg dose cap is NOT natively supported (no `max_dose` parameter). Prednisone (oral, days 1-5) modeled as a separate regimen. Workaround for vincristine: pre-calculate capped dose and use flat dosing.

### 3.2 mFOLFOX6 (Colorectal Cancer)

| Drug | Dose | Basis | Days | Route |
|------|------|-------|------|-------|
| Oxaliplatin | 85 mg/m2 | BSA | Day 1 (2h) | IV |
| Leucovorin | 400 mg/m2 | BSA | Day 1 (2h) | IV |
| 5-FU bolus | 400 mg/m2 | BSA | Day 1 (push) | IV |
| 5-FU infusion | 2,400 mg/m2 over 46h | BSA | Days 1-2 (continuous) | IV |

- **Cycle length:** 14 days, 6-12 cycles

**Verdict: PARTIAL** -- oxaliplatin and leucovorin on Day 1 work. 5-FU is given as both bolus AND 46-hour continuous infusion, which cannot be represented as one drug. Workaround: model bolus 5-FU and infusional 5-FU as two separate "drugs" with different names.

### 3.3 ABVD (Hodgkin Lymphoma)

| Drug | Dose | Basis | Days | Route |
|------|------|-------|------|-------|
| Doxorubicin | 25 mg/m2 | BSA | Days 1, 15 | IV |
| Bleomycin | 10 units/m2 | BSA | Days 1, 15 | IV |
| Vinblastine | 6 mg/m2 | BSA | Days 1, 15 | IV |
| Dacarbazine | 375 mg/m2 | BSA | Days 1, 15 | IV |

- **Cycle length:** 28 days, 2-6 cycles

**Verdict: YES** -- all drugs share `admin_days = c(1, 15)`, all BSA-based. Clean use of `calculate_combination_costs()`.

### 3.4 Cisplatin + Gemcitabine (Bladder/NSCLC)

| Drug | Dose | Basis | Days | Route |
|------|------|-------|------|-------|
| Gemcitabine | 1,000 mg/m2 | BSA | Days 1, 8 | IV |
| Cisplatin | 70 mg/m2 | BSA | Day 1 | IV |

- **Cycle length:** 21 days, 4-6 cycles

**Verdict: YES** -- different `admin_days` per drug within the same cycle work. Gemcitabine `admin_days = c(1, 8)`, cisplatin `admin_days = c(1)`. Both BSA-based.

---

## 4. Mixed-Route Combinations

### 4.1 CAPOX/XELOX (Colorectal Cancer)

| Drug | Route | Dose | Basis | Days |
|------|-------|------|-------|------|
| Oxaliplatin | IV | 130 mg/m2 | BSA | Day 1 |
| Capecitabine | Oral (BID) | 1,000 mg/m2 per dose | BSA | Days 1-14 |

- **Cycle length:** 21 days, up to 8 cycles (adjuvant)

**Verdict: YES** -- two separate regimen objects (IV + oral) combined via `calculate_combination_costs()`. Capecitabine BID handled with `admin_days = rep(1:14, each = 2)`.

### 4.2 Trastuzumab + Capecitabine (HER2+ mBC)

| Drug | Route | Dose | Basis | Days |
|------|-------|------|-------|------|
| Trastuzumab | IV | Loading 8 mg/kg; Maint 6 mg/kg | Weight | Day 1 |
| Capecitabine | Oral (BID) | 1,000 mg/m2 per dose | BSA | Days 1-14 |

- **Cycle length:** 21 days

**Verdict: PARTIAL** -- trastuzumab loading dose not supported in single regimen. Maintenance phase fully modelable.

### 4.3 Atezolizumab + Nab-Paclitaxel (IMpassion130, TNBC)

| Drug | Route | Dose | Basis | Days |
|------|-------|------|-------|------|
| Atezolizumab | IV | 840 mg | Flat | Days 1, 15 |
| Nab-paclitaxel | IV | 100 mg/m2 | BSA | Days 1, 8, 15 |

- **Cycle length:** 28 days

**Verdict: YES** -- both IV with different admin days. Atezolizumab `admin_days = c(1, 15)`, nab-paclitaxel `admin_days = c(1, 8, 15)`.

### 4.4 Nivolumab + Ipilimumab (CheckMate-067, Melanoma)

**Induction (4 doses, Q3W):**

| Drug | Dose | Basis | Days |
|------|------|-------|------|
| Nivolumab | 1 mg/kg | Weight | Day 1 |
| Ipilimumab | 3 mg/kg | Weight | Day 1 |

**Maintenance:** Nivolumab 240 mg Q2W or 480 mg Q4W (flat dose, monotherapy)

- **Cycle length:** 21 days (induction), 14 or 28 days (maintenance)

**Verdict: PARTIAL** -- induction (both drugs Q3W x 4, `max_med_cycles = 4`) is fully modelable. Transition to nivolumab-only maintenance at a different dose, schedule, and dose basis requires separate regimen objects.

### 4.5 Bendamustine + Rituximab (BR, Indolent NHL)

| Drug | Route | Dose | Basis | Days |
|------|-------|------|-------|------|
| Rituximab | IV | 375 mg/m2 | BSA | Day 1 |
| Bendamustine | IV | 90 mg/m2 | BSA | Days 1, 2 |

- **Cycle length:** 28 days, 6 cycles

**Verdict: YES** -- rituximab `admin_days = c(1)`, bendamustine `admin_days = c(1, 2)`. Both BSA-based.

### 4.6 Pembrolizumab + Lenalidomide + Dexamethasone (KEYNOTE-185, MM)

**Note:** Trial halted due to safety concerns. Not approved. Included for structural interest.

| Drug | Route | Dose | Basis | Days |
|------|-------|------|-------|------|
| Pembrolizumab | IV | 200 mg flat | Flat | Q3W (21-day cycle) |
| Lenalidomide | Oral | 25 mg daily | Flat | Days 1-21 (28-day cycle) |
| Dexamethasone | Oral | 40 mg | Flat | Days 1, 8, 15, 22 (28-day cycle) |

**Verdict: PARTIAL** -- the misalignment between pembrolizumab's 21-day cycle and the 28-day oral cycle is the challenge. Pembrolizumab must be modeled as its own 21-day regimen separately; `calculate_combination_costs()` requires aligned `model_cycle_length`/`n_cycles`.

---

## 5. Complex / Edge-Case Schedules

### 5A. Loading Doses (Different First Dose)

#### 5A.1 Pertuzumab + Trastuzumab (HER2+ Breast)

- Pertuzumab: 840 mg Cycle 1, then 420 mg Q3W
- Trastuzumab: 8 mg/kg Cycle 1, then 6 mg/kg Q3W
- Both drugs have a loading dose that is double the maintenance dose.

**Verdict: NO** (single regimen) -- `dose_per_admin` is fixed. Requires separate regimen objects per phase.

#### 5A.2 PHESGO (SC pertuzumab + trastuzumab)

- Loading: pertuzumab 1,200 mg + trastuzumab 600 mg + hyaluronidase SC
- Maintenance: pertuzumab 600 mg + trastuzumab 600 mg + hyaluronidase SC Q3W
- Two different product presentations (loading vs maintenance syringe)

**Verdict: NO** (single regimen) -- loading dose change + route is SC (not supported as a distinct route, though could be approximated as IV with single-use vial).

#### 5A.3 Obinutuzumab (CLL, with venetoclax)

- Cycle 1: Day 1 = 100 mg IV; Day 2 = 900 mg IV; Day 8 = 1,000 mg IV; Day 15 = 1,000 mg IV
- Cycles 2-6: 1,000 mg IV Day 1 only
- Split first dose (100/900) for infusion reaction management.

**Verdict: NO** -- three different dose levels within cycle 1, plus different admin days in cycle 1 vs subsequent cycles.

### 5B. Dose Escalation Over Time

#### 5B.1 Venetoclax Ramp-Up (CLL)

- Week 1: 20 mg/day; Week 2: 50 mg; Week 3: 100 mg; Week 4: 200 mg; Week 5+: 400 mg
- Five distinct dose levels, each lasting 1 week.

**Verdict: NO** -- dose escalation not supported. Would require 5 separate regimen objects.

#### 5B.2 Carfilzomib (Multiple Myeloma)

- Cycle 1, Days 1-2: 20 mg/m2 IV
- Cycle 1, Days 8, 9, 15, 16: escalated to 27 mg/m2 (or 56 mg/m2)
- Cycles 2-12: 27 mg/m2 on Days 1, 2, 8, 9, 15, 16
- Cycle 13+: 27 mg/m2 on Days 1, 2, 15, 16 (days 8, 9 dropped)

**Verdict: NO** -- within-cycle dose escalation AND schedule changes across cycle phases.

### 5C. Induction + Maintenance Phase Changes

#### 5C.1 Daratumumab (Multiple Myeloma)

- Dose: 16 mg/kg IV throughout (constant)
- Weeks 1-8: Weekly (8 doses)
- Weeks 9-24: Every 2 weeks (8 doses)
- Week 25+: Every 4 weeks until progression
- Three distinct frequency phases, same dose.

**Verdict: NO** (single regimen) -- dosing frequency de-escalation not supported. Requires 3 separate regimen objects.

#### 5C.2 Bortezomib VMP (Multiple Myeloma)

- Cycles 1-4 (42-day cycles): 1.3 mg/m2 SC Days 1, 4, 8, 11, 22, 25, 29, 32
- Cycles 5-9 (35-day cycles): 1.3 mg/m2 SC Days 1, 8, 15, 22
- Cycle length changes AND dose frequency drops.

**Verdict: NO** -- changing cycle length and admin schedule across phases.

#### 5C.3 Lenalidomide Maintenance (Post-ASCT, MM)

- Initial: 10 mg daily Days 1-21 of 28-day cycles
- After 3 cycles: may increase to 15 mg daily
- Conditional dose change.

**Verdict: NO** (single regimen) -- conditional mid-treatment dose increase.

#### 5C.4 Rituximab Induction then Maintenance (Follicular Lymphoma)

- Induction: 375 mg/m2 IV Day 1 Q3W x 8 cycles
- Maintenance: 375 mg/m2 IV every 2 months for up to 2 years
- Schedule shifts from 21-day to 56-day cycle.

**Verdict: NO** (single regimen) -- cycle length change. Two separate regimen objects needed.

#### 5C.5 Temozolomide Stupp Protocol (Glioblastoma)

- Concurrent (with RT): 75 mg/m2 daily x 42 consecutive days
- Adjuvant Cycle 1: 150 mg/m2 Days 1-5 of 28-day cycle
- Adjuvant Cycles 2-6: 200 mg/m2 Days 1-5 of 28-day cycle
- Three distinct dose levels and two fundamentally different schedules.

**Verdict: NO** -- requires at least 3 separate regimen objects.

### 5D. Complex Within-Cycle Schedules

#### 5D.1 mFOLFOX6 5-FU Component

- Same drug given as bolus (400 mg/m2 IV push) AND 46-hour continuous infusion (2,400 mg/m2).
- Package cannot model the same drug with two delivery methods.

**Verdict: NO** (as single drug) -- workaround: model as two separate "drugs."

### 5E. Continuous Daily Oral (No Cycles)

#### 5E.1 Imatinib (CML)

- 400 mg daily continuously. May escalate to 600/800 mg.

**Verdict: YES** (at fixed dose) -- conditional escalation not supported.

#### 5E.2 Osimertinib (EGFR+ NSCLC)

- 80 mg daily continuously.

**Verdict: YES**

#### 5E.3 Ibrutinib (CLL)

- 420 mg daily continuously.

**Verdict: YES**

### 5F. Subcutaneous Formulations

#### 5F.1 Rituximab SC (MabThera SC)

- Follicular/DLBCL: 1,400 mg rituximab + hyaluronidase SC
- CLL: 1,600 mg rituximab + hyaluronidase SC
- First dose must be IV; subsequent switch to SC.

**Verdict: NO** -- SC route not supported; mid-treatment IV-to-SC switch not supported.

#### 5F.2 Trastuzumab SC

- Fixed dose: 600 mg SC Q3W regardless of body weight.
- No loading dose (unlike IV).

**Verdict: PARTIAL** -- could approximate as IV with single-use "vial" of 600 mg. SC route not explicitly supported but costing logic would work.

#### 5F.3 PHESGO (SC pertuzumab + trastuzumab)

See 5A.2 above.

**Verdict: NO** -- loading dose + SC route.

---

## Summary: Capability Matrix

| Feature Needed | Supported? | Regimens Requiring It |
|----------------|-----------|----------------------|
| Flat dose, single admin day | YES | Pembrolizumab, atezolizumab, avelumab, durvalumab |
| Weight-based dosing (mg/kg) | YES | Nivolumab, bevacizumab, ipilimumab, panitumumab, T-DM1 |
| BSA-based dosing (mg/m2) | YES | Rituximab, cetuximab, ABVD drugs, gemcitabine |
| Multiple vial sizes with optimization | YES | Nivolumab (4 sizes), bevacizumab, rituximab |
| Per-vial-size pricing | YES | Nivolumab, durvalumab, ipilimumab |
| Oral with on/off schedule | YES | Lenalidomide, palbociclib, ribociclib, sunitinib |
| Continuous daily oral | YES | Ibrutinib, osimertinib, enzalutamide, olaparib |
| Multiple tablet strengths | YES | Capecitabine (150/500 mg), venetoclax (10/50/100 mg) |
| Treatment duration cap | YES | Pembrolizumab (35 cycles), ipilimumab (4 doses) |
| Multi-drug combinations | YES | ABVD, cisplatin+gemcitabine, BR, CAPOX |
| BID dosing | YES (workaround) | Capecitabine, olaparib |
| Different admin days per drug | YES | Cisplatin+gemcitabine, atezolizumab+nab-paclitaxel |
| SC/IM route | YES | Bortezomib SC, trastuzumab SC, PHESGO |
| Loading dose (different first dose) | YES (multi-regimen) | Trastuzumab, pertuzumab, cetuximab, obinutuzumab |
| Dose escalation/ramp-up | YES (multi-regimen) | Venetoclax, carfilzomib |
| Phase-dependent schedule changes | YES (multi-regimen) | Daratumumab, bortezomib VMP, temozolomide Stupp |
| Changing cycle length mid-treatment | YES (multi-regimen) | Bortezomib VMP, rituximab induction/maintenance |
| Dose cap (max mg per admin) | YES (pre-calculate) | Vincristine (max 2 mg in R-CHOP) |
| Same drug as bolus + infusion | YES (two drug entries) | 5-FU in FOLFOX |
| Mid-treatment route switching | YES (multi-regimen) | Rituximab IV to SC |
| Conditional dose changes | YES (multi-regimen) | Lenalidomide maintenance increase |

---

## Regimen Count by Modelability

| Status | Count | Notes |
|--------|-------|-------|
| Fully modelable (YES) | 43 | Includes multi-regimen composition and BID workaround cases |
| **Total** | **43** | |
