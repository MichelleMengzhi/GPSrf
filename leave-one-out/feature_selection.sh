#!/bin/sh

### use test selected features 
# baseline
cat $1$2_baseline_overlap.pop.txt | grep -E 'NorthEastAsian|Mediterranean|SouthAfrican|SouthWestAsian|NativeAmerican|Oceanian|SouthEastAsian|NorthernEuropean|SubsaharanAfrican' | cut -f1 -d' ' >> $1$2_baseline_overlap.pop

~/admixture32 $1$2_baseline_overlap.bed -F 9 -j8
mv $2_baseline_overlap* $1/
cat $1$2_baseline_overlap.fam | cut -d ' ' -f1-2 > $1$2_out_ind_id
sed -i 's/ /\t/g' $1$2_out_ind_id
sed -i 's/ /\t/g' $1$2_baseline_overlap.9.Q
paste $1$2_out_ind_id $1$2_baseline_overlap.9.Q > $1out_Q_$2_baseline
sed -i '1 i\Populations\tGRC\tMediterranean\tNative American\tNortheast Asian\tNorthern European\tOceanian\tSouthern African\tSoutheast Asian\tSouthwest Asian\tSubsaharan African'  $1out_Q_$2_baseline

# benchmark
#plink --bfile $1$2_baseline_overlap --extract sample_feature/benchmark.snp  --make-bed --out $1$2_selected_bench --noweb

#cut -f1-2 -d ' ' $1$2_selected_bench.fam > $1$2_selected_bench.pop.txt
#printf '%.0s\n' {1..$3}  > $1$2_selected_bench.pop
wc -l $1$2_selected_bench.pop

cat $1$2_selected_bench.pop.txt | grep -E 'NorthEastAsian|Mediterranean|SouthAfrican|SouthWestAsian|NativeAmerican|Oceanian|SouthEastAsian|NorthernEuropean|SubsaharanAfrican' | cut -f1 -d' ' >> $1$2_selected_bench.pop
wc -l $1$2_selected_bench.pop

~/admixture32 $1$2_selected_bench.bed -F 9 -j8
mv $2_selected_bench* $1/
cat $1$2_selected_bench.fam | cut -d ' ' -f1-2 > $1$2_out_ind_id_bench
sed -i 's/ /\t/g' $1$2_out_ind_id_bench
sed -i 's/ /\t/g' $1$2_selected_bench.9.Q
paste $1$2_out_ind_id_bench $1$2_selected_bench.9.Q > $1out_Q_$2_bench
sed -i '1 i\Populations\tGRC\tMediterranean\tNative American\tNortheast Asian\tNorthern European\tOceanian\tSouthern African\tSoutheast Asian\tSouthwest Asian\tSubsaharan African'  $1out_Q_$2_bench

# split300

#plink --bfile $1$2_baseline_overlap --extract sample_feature/socres_df.split.top300.snp  --make-bed --out $1$2_selected_split300 --noweb

#cut -f1-2 -d ' ' $1$2_selected_split300.fam > $1$2_selected_split300.pop.txt
#printf '%.0s\n' {1..$3}  > $1$2_selected_split300.pop

cat $1$2_selected_split300.pop.txt | grep -E 'NorthEastAsian|Mediterranean|SouthAfrican|SouthWestAsian|NativeAmerican|Oceanian|SouthEastAsian|NorthernEuropean|SubsaharanAfrican' | cut -f1 -d' ' >> $1$2_selected_split300.pop

~/admixture32 $1$2_selected_split300.bed -F 9 -j8
mv $2_selected_split300* $1/
cat $1$2_selected_split300.fam | cut -d ' ' -f1-2 > $1$2_out_ind_id_split300
sed -i 's/ /\t/g' $1$2_out_ind_id_split300
sed -i 's/ /\t/g' $1$2_selected_split300.9.Q
paste $1$2_out_ind_id_split300 $1$2_selected_split300.9.Q > $1out_Q_$2_split300
sed -i '1 i\Populations\tGRC\tMediterranean\tNative American\tNortheast Asian\tNorthern European\tOceanian\tSouthern African\tSoutheast Asian\tSouthwest Asian\tSubsaharan African'  $1out_Q_$2_split300


