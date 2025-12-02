
# file paths
# ase_fp = "~/projects/visitor_sse/output/logs/A2/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/A2/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_equal_rates_new_priors/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_rates_new_priors/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_new_priors_burnin/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_new_priors_rows/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_rates_fixed_sampling/"
# ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_new_priors_4times/"
ase_fp = "~/Documents/Code\ Testing/visitor_sse/output/logs/full/visitor_unequal_new_priors_40times/"

# ase_fn = "out._A2.seed_5.ancStates"
ase_fn = "out._full.seed_5.ancStates"

in_ase_fn = paste0( ase_fp, ase_fn ) 
out_home_fn = paste0( ase_fp, ase_fn, "_home" )
out_curr_fn = paste0( ase_fp,ase_fn, "_curr" )
out_match_fn = paste0( ase_fp,ase_fn, "_match" )

# read in ancestral state trace
df_orig  = read.table(in_ase_fn, sep="\t", header=T)
df_home  = df_orig
df_curr  = df_orig
df_match = df_orig

# make state conversion table
combo = c(0:9, LETTERS[1:15])
curr  = rep(0:4, 5)
home  = sort(curr)

states = data.frame( combo=combo, home=home, curr=curr )
states$match = ifelse(states$home == states$curr, 1, 0)
print(states)

# convert ancestral states to home/curr locations
for (i in 1:nrow(df_orig)) {

    # convert row
    row_orig  = df_orig[i,2:ncol(df_orig)]
    row_home  = states$home[ match(row_orig, states$combo) ]
    row_curr  = states$curr[ match(row_orig, states$combo) ]
    row_match = states$match[ match(row_orig, states$combo) ]

    # store row in table
    df_home[i,2:ncol(df_home)] = row_home
    df_curr[i,2:ncol(df_home)] = row_curr
    df_match[i,2:ncol(df_home)] = row_match
}

print(states)
cat("original states\n")
print(row_orig[1:10])
cat("home state of lineage\n")
print(row_home[1:10])
cat("curr state of lineage\n")
print(row_curr[1:10])
cat("match state of lineage\n")
print(row_match[1:10])

print("Writing files...")
write.table(x=df_home, file=out_home_fn, quote=F, sep="\t", row.names=F, col.names=T)
write.table(x=df_curr, file=out_curr_fn, quote=F, sep="\t", row.names=F, col.names=T)
write.table(x=df_match, file=out_match_fn, quote=F, sep="\t", row.names=F, col.names=T)
