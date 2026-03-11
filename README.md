# 공공데이터 특성을 반영한 재현데이터 생성방법 연구

본 저장소는 논문 「공공데이터 특성을 반영한 재현데이터 생성방법 연구」의 재현을 위한 코드와 결과 파일을 포함하고 있다.

## Repository Structure
```
│
├── code/                 # 재현데이터 생성 및 분석 실행 코드
│   ├── 01_prepare_data.R
│   ├── 02_synthesis.R
│   ├── 03_summary.R
│   └── functions.R
│
├── materials/
│   └── rule_definition.md   # 원자료 변수 간 제약조건 정리
│   └── session_info.txt          # 분석 실행 환경 정보
│   └── codebook.xlsx             # 분석에 사용된 변수 설명
│
├── results/              # 논문에 보고된 최종 표 및 그림
│   ├── pMSE_results.rda
│   ├── table_dist_based.csv
│   ├── table_expenditure_mean.csv
│   └── figures/
│       ├── Fig_dist_based.png
│       ├── Fig_sum_constraint.png
│       ├── Figure_consum_compare.png
│       ├── Fig_PCC.png
│       └── Fig_PS.png
│
└── data/
    ├── raw/              # 원자료 저장 위치 (사용자가 직접 다운로드)
    └── processed/        # 전처리된 데이터
```

## Reproducibility
논문의 분석 결과를 재현하기 위해 다음 순서로 코드를 실행한다.

1. code/01_prepare_data.R
- MDIS에서 다운로드한 원자료를 불러와 분석에 필요한 변수만 정리하고 전처리된 데이터를 생성한다.

1. code/02_synthesis.R
- 전처리된 원자료를 이용하여 재현데이터를 생성한다.

1. code/03_summary.R
- 재현데이터의 유용성과 노출위험을 평가하고 논문에 보고된 표와 그림을 생성한다.

## Data Access

본 연구에 사용된 가계동향조사 원자료는 MDIS(Microdata Integrated Service, https://mdis.kostat.go.kr)에서 다운로드할 수 있다.

다운로드한 원자료는 다음 경로에 저장해야 한다.
```
data/raw/household_2024.csv
```
