# NARA-2020 achtergronddocumenten

**Dit repo is bevroren en gearchiveerd (beslissing 03/09/2026).** De NARA-2020-editie is afgesloten en de rapporten worden nooit meer her-gerenderd. Latere edities van het Natuurrapport hebben een eigen repo.

- De branch `publish` bevat de definitieve, gepubliceerde HTML. De INBO-website (www.vlaanderen.be/inbo) importeert die branch elke 30 minuten in haar CMS. Wijzig deze branch niet en render ze niet opnieuw. Ze is beschermd tegen push, force-push en verwijdering.
- De workflow `render-publish` is bewust uitgeschakeld. Zet ze niet opnieuw aan. De workflows verwijzen bovendien naar `inbo/actions/render_nara@master`, een action die niet meer bestaat.
- Het Docker-image (`inbobmk/nara2020`, pandoc 2.7.3, R 4.1.0) wordt niet meer onderhouden.

Achtergrond: in juli 2026 bleek dat de gepubliceerde rapporten een script laadden van polyfill.io, een CDN dat in 2024 gekaapt werd (INBO security incident SecID_20260702). De regel stond hardgecodeerd in `template/default.html` en is verwijderd uit `main` en uit de `publish`-branch. Zie inbo/nara-2020#54.
