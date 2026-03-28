
all: data results \
	data/games_sample.RDS data/wrangled_table.RDS data/wrangled_table.csv \
	results/class_distribution_plot.png \
	results/numeric_feature_distributions.png \
	results/target_by_release_binary.png \
	results/categorical_feat_gap.png \
	results/roc_curve.png results/confusion_matrix.png \
	results/evaluation_metrics_table.csv \
	results/feature_importances_table.csv \
	reports/steam_full_analysis.html \
	reports/steam_full_analysis.pdf


# make directories in case they do not exist already

data:
	mkdir -p data

results:
	mkdir -p results

#data

data/games_sample.RDS: scripts/01_download-data.R 
	Rscript scripts/01_download-data.R input_url data/


data/wrangled_table.RDS data/wrangled_table.csv: \
	scripts/02_data-preprocessing.R data/games_sample.RDS 
	Rscript scripts/02_data-preprocessing.R data/ data/ data/


#results 

results/class_distribution_plot.png: \
	scripts/03_class-imbalance-check.R src/io_validation_utils.R src/plot_class_imbalance.R data/wrangled_table.RDS 
	Rscript scripts/03_class-imbalance-check.R data/ results/ results/

results/numeric_feature_distributions.png: \
	scripts/04_numeric-features-distributions.R src/io_validation_utils.R src/plot_numeric_distributions.R data/wrangled_table.RDS 
	Rscript scripts/04_numeric-features-distributions.R data/ results/ results/

results/target_by_release_binary.png: \
	scripts/05_additional-target-summary-plots.R data/wrangled_table.RDS 
	Rscript scripts/05_additional-target-summary-plots.R data/ results/ results/

results/categorical_feat_gap.png: \
	scripts/06_categorical-features-plots.R data/wrangled_table.RDS 
	Rscript scripts/06_categorical-features-plots.R data/ results/ results/

results/roc_curve.png results/confusion_matrix.png \
results/evaluation_metrics_table.csv results/feature_importances_table.csv: \
	scripts/07_train-test-model.R data/wrangled_table.RDS 
	Rscript scripts/07_train-test-model.R data/ results/

# reports

reports/steam_full_analysis.html: \
	reports/steam_full_analysis.qmd \
	results/class_distribution_plot.png \
	results/numeric_feature_distributions.png \
	results/target_by_release_binary.png \
	results/categorical_feat_gap.png \
	results/roc_curve.png \
	results/confusion_matrix.png \
	results/evaluation_metrics_table.csv \
	results/feature_importances_table.csv
	quarto render reports/steam_full_analysis.qmd --to html

reports/steam_full_analysis.pdf: \
	reports/steam_full_analysis.qmd \
	results/class_distribution_plot.png \
	results/numeric_feature_distributions.png \
	results/target_by_release_binary.png \
	results/categorical_feat_gap.png \
	results/roc_curve.png \
	results/confusion_matrix.png \
	results/evaluation_metrics_table.csv \
	results/feature_importances_table.csv
	quarto render reports/steam_full_analysis.qmd --to pdf

# clean

clean:
	rm -rf data/games_sample.RDS data/wrangled_table.RDS data/wrangled_table.csv
	rm -rf results
	rm -rf reports/steam_full_analysis.html reports/steam_full_analysis.pdf

test:
	Rscript tests/testthat.R
