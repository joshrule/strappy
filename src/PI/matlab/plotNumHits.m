function plotNumHits(files)

f = figure; hold on;
for file = files(:)
    file
    d = load(file);
    plot d;
end