registrydir=/your/registry/path
corporadir=/your/data/path

mkdir -p $corporadir/vie_fr;        mkdir -p $corporadir/vie_ru;
cwb-encode -c utf8 -d $corporadir/vie_fr -f vie_fr.cqp -R $registrydir/vie_fr -P pos -P lemma -S tu+id -S text+id -S corpus+id
cwb-make -r $registrydir VIE_FR
cwb-encode -c utf8 -d $corporadir/vie_ru -f vie_ru.cqp -R $registrydir/vie_ru -P pos -P lemma -S tu+id -S text+id -S corpus+id
cwb-make -r $registrydir VIE_RU
# -e et -p options should be removed when the corpus will be cleaned
cwb-align-import -r $registrydir -e -p -v vie_align.cqp
cwb-align-import -r $registrydir -e -p -v -inverse vie_align.cqp
