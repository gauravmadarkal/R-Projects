quaddata_renew$percentageVariation = (100 - ((quaddata_renew$V/230) * 100))
quaddata_renew$percentageVariation = abs(quaddata_renew$percentageVariation)
