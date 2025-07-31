source activate picrust2
picrust2_pipeline.py --version
picrust2_pipeline.py -s otus_rep.fasta -i otu_table.biom -o picrust2_output -p 4
pathway_pipeline.py  -i pred_metagenome_unstrat.tsv -o KEGG_pathways_out --no_regroup --map ./KEGG_pathways_to_KO.tsv