# Notas de presentación — IIFET 2026, Tórshavn

**Paper:** *Are fishing decisions flexible? Participation, species target, and landing location choices in the U.S. West Coast CPS fishery.*
Quezada-Escalona, Tommasi, Kaplan, Muhling & Stohs — *Ecological Economics* **247** (2026).

**Sesión:** "Spatial Trade-offs of Ecosystem-based Management: Models, Theory, and Performance (Part 2)" — jueves 20 ago, 13:30–15:00, Dansistovan.
**Formato real:** 7 papers en 90 min ⇒ ~12 min por slot. Hablamos **últimos**, así que hay que asumir que la sesión va atrasada.
**Objetivo:** 10 min de charla + 2 de preguntas. Plan B: 6–7 min (ver §Plan comprimido).

> Cómo usar este documento: la estructura y los comentarios están en español; **las líneas entre comillas son lo que se dice en inglés**, tal cual. No hay que memorizarlas palabra por palabra — sí memorizar la *primera frase* y la *frase de transición* de cada slide, que es donde uno se traba.

---

## Reloj de la charla

| # | Slide | Tiempo | Acumulado |
|---|-------|--------|-----------|
| 1 | Título | 0:20 | 0:20 |
| 2 | Motivation | 0:50 | 1:10 |
| 3 | **The contribution (hero)** | 1:30 | 2:40 |
| 4 | Why that swap matters | 0:55 | 3:35 |
| 5 | Setting | 0:40 | 4:15 |
| 6 | The model | 0:50 | 5:05 |
| 7 | Drivers (tabla) | 1:00 | 6:05 |
| 8 | **Scenario (payoff)** | 1:20 | 7:25 |
| 9 | Substitution `[CUT]` | 0:45 | 8:10 |
| 10 | Takeaways | 0:55 | 9:05 |
| 11 | Thanks | 0:15 | 9:20 |

Los dos slides que **no** se sacrifican son el **3** (la contribución) y el **8** (el payoff). Todo lo demás es negociable.

---

## Slide 1 — Título

**Idea única:** quién soy y de qué va esto, en una frase.

**Guion:**

> "Thank you. I'm Felipe Quezada, from Universidad de Concepción, and this is joint work with colleagues at NOAA. The paper asks a simple question: **when the ocean changes, how flexible are fishing decisions?** And the way we answer it is by taking a model that oceanographers built — a species distribution model — and using its daily output as the central economic regressor."

**Cuidado:** no leer la lista de coautores ni los agradecimientos. 20 segundos, se pasa.

**Transición:** "Let me start with why the *portfolio* of species is the right place to look."

---

## Slide 2 — Motivation: los trade-offs espaciales pasan por el portafolio de especies

**Idea única:** el resultado espacial no se decide sólo en el espacio — se decide en *a qué especie te puedes cambiar*.

**Guion:**

> "Fishable area is shrinking — offshore wind, conservation closures, mining — while the species themselves are moving with climate. For management, what actually matters is not the shock, it's **how the fleet redistributes effort**: they can follow the species, switch species, switch ports, stay in port, or leave the fishery altogether.
>
> The problem is that location-choice models almost always model **one fishery at a time**. When you do that, the species portfolio drops out of the model by construction — and with it, most of the adaptation margin.
>
> So our question is: how do **availability, prices, costs and weather** shape the **daily** decision to participate, to target a species, and to land at a port? And we ask it in a period with two big natural experiments: the **2015 sardine closure** and the **2014–2016 marine heatwave**."

**Puntero:** la columna derecha, las tres palabras en gold (*participate, target, land*).

**Transición (memorizar):** "So we model those three decisions **jointly** — and the input that makes that possible is on the next slide."

---

## Slide 3 — La contribución (HERO) ⭐

**Idea única:** *el modelo de los oceanógrafos se convierte en el regresor de los economistas.*
Éste es **el** slide de la charla. Si sólo se entiende uno, que sea éste. Ir más lento aquí que en cualquier otro.

**Guion:**

> "Here is what we do differently. Oceanographers build species distribution models — SDMs — for **ecological** purposes: they take the ocean state from a ROMS model, sea surface temperature, sea surface height, chlorophyll, and fit a GAM that predicts the **probability of presence** of a species on a spatial grid, day by day. That's Muhling and co-authors, 2019 and 2020.
>
> [pausa, seguir la flecha] We take that daily field and average it **within a species-specific radius of each port**. That gives us **Availability**: an **exogenous, gap-free** measure of how much of each species is around **each port, for each species, on each day**. And that variable enters the fisher's utility function directly.
>
> One sentence on the assumption: we're assuming **habitat suitability tracks potential catch rates**. If it only does so partly, our availability effect is **attenuated** — biased toward zero, not inflated."

**Frase para decir literal, mirando a la audiencia (no a la pantalla):**

> "In one line: **the oceanographers' model becomes the economists' regressor.**"

**Puntero:** recorrer el pipeline izquierda→derecha, arriba→abajo (Ocean state → GAM SDM → daily grid → port radius), y terminar en la caja dorada.

**Cuidado:**

- El mapa de la derecha **no es output nuestro** — es ilustrativo, de Chasco et al. (2022). Decirlo si alguien pregunta: *"that map is illustrative, it's not our own SDM output — it's there to show what a habitat field looks like."*
- **No** decir que el SDM predice *mejor* la captura que el catch pasado. No hicimos esa comparación (ver §Q&A #1).

**Transición:** "Why is that swap worth doing? Three reasons."

---

## Slide 4 — Por qué importa el cambio: catch pasado vs. SDM

**Idea única:** el catch pasado es endógeno, sesgado y con huecos; el SDM no — y sobre todo, el SDM se puede *mover*.

**Guion:** leer sólo las palabras en negrita, no las viñetas completas.

> "The standard proxy for availability is **past catch** — CPUE from previous trips or previous years. Three problems. It's **endogenous**: it records where vessels already *chose* to fish, and that choice is exactly what we're modeling. It's **selection-biased**: you only observe it at sites that were fished — Chen and co-authors make this point in 2023. And it has **gaps**: it's simply undefined for a port–day when nobody went out.
>
> The SDM index has none of those. It's an **environmental input**, so it's exogenous to the vessel's decision. It has **a value for every port–day**, including the days nobody fished. And — this is the one that buys us the second half of the talk — it's **counterfactual-ready**: you can move the habitat field and re-predict."

**Frase de cierre del slide (importante, engancha con el payoff):**

> "**You cannot shift a 'past catch' field.** That's the whole reason we can run a spatial scenario at all."

**Transición:** "Quick word on the setting, and then the results."

---

## Slide 5 — Setting: cuatro segmentos de flota, cuatro portafolios

**Idea única:** la unidad de adaptación no es "la pesquería", es el **segmento de flota** — cada uno con un portafolio distinto.

**Guion:** rápido, 40 s. No leer la tabla.

> "This is the U.S. West Coast Coastal Pelagic Species fishery — market squid, sardine, anchovy, mackerels, plus non-CPS options like Dungeness crab, salmon and tuna. Purse seine.
>
> We don't treat it as one fleet. From a cluster analysis in earlier work we get **four segments**: a large southern **squid specialist** — that's the bulk of the data, about 29,000 trips; a **roving squid–sardine generalist**; a **Pacific Northwest sardine specialist**; and a small southern **forage-fish diverse** segment. Four segments, four different portfolios. Same shock, different room to manoeuvre."

**Puntero:** sólo la columna "Main target(s)".

**Números por si preguntan:** ~41,000 trips en total (29,160 / 6,806 / 2,581 / 2,481); 2013–2017; los segmentos vienen de Quezada et al. (2024).

**Transición:** "The model is a standard nested logit — one slide, no derivations."

---

## Slide 6 — El modelo

**Idea única:** una sola línea de utilidad; lo único que hay que retener es que **Avail entra diario y es exógena**.

**Guion:** (comprimible a 25 s si vamos tarde)

> "Each alternative is a **triple**: participate, target species, landing port. Utility is linear: an alternative-specific constant, our **availability term** — coefficient allowed to vary by species — expected price, plus distances, wind, closures and local unemployment. And a **state-dependence** term: whether the vessel made that same choice in the last thirty days. That picks up processor ties and port capacity — the fact that these vessels have contracts, not just preferences.
>
> We nest first on **participation**, then on **species** — except for the forage-diverse segment, where the data prefer nesting by **port**. That difference matters later. Crab and salmon are split off, because switching gear there is costly. Estimated with Apollo in R; the dominant species defines the target, and that's 93.6% of trip revenue, so it's not a coarse assignment."

**Cuidado:** no derivar nada, no explicar la fórmula del nested logit, no mencionar GEV. Si alguien quiere, está en el backup de λ.

**Transición:** "So: what actually drives the choice?"

---

## Slide 7 — Drivers: coeficientes de disponibilidad

**Idea única:** la señal del SDM sobrevive a una inercia muy fuerte ⇒ la flexibilidad que medimos es real.

**Guion:**

> "Rows are the availability of each species; columns are the four fleet segments. Read **signs and significance**, not magnitudes — these are utility units, so they're not comparable across segments; magnitude comes from the scenario in two slides.
>
> Squid availability is strongly positive wherever squid is relevant. Anchovy is positive for three segments. Non-CPS availability — crab and salmon — is large and positive for the two segments that have those options: those species **gate participation**.
>
> Now look at the **bottom two rows**, because that's the whole argument. **Prices** are positive and significant for three of four segments. And **state dependence is big** — between 2.3 and 3.1 everywhere. These fleets are **habitual**: contracts, processors, capacity."

**Frase clave (decirla mirando a la audiencia):**

> "And yet, *even with that much inertia in the model*, the **SDM signal survives**. That's what tells us the flexibility we measure is genuine behaviour, not just persistence."

**Puntero:** las **dos filas de abajo**. No pasear el puntero por toda la tabla.

**Si preguntan por el anchovy negativo del forage-diverse (−1.88):** está en la nota al pie — la disponibilidad de anchoveta y de calamar están **correlacionadas negativamente** (r = −0.29), así que ese coeficiente recoge el costo de oportunidad, no aversión a la anchoveta.

**Transición (memorizar):** "Signs are one thing. Here's what it's worth in space."

---

## Slide 8 — El payoff: mover el campo de hábitat ⭐

**Idea única:** movemos el hábitat del calamar y el modelo re-predice **dónde termina el esfuerzo** — y el total por puerto **esconde una recomposición de especies**.

**Guion:**

> "This is what the SDM input buys you. We take the squid habitat field and swap it for a **later window within the same season** — the habitat centroid moves south, from about **43 degrees North to 40.7**. Everything else is held fixed: prices, costs, weather, the other species. Then we let the model re-predict.
>
> Each panel is a fleet segment; bars are the change in the **probability of landing at each port**, in percentage points. Gold gains effort, blue loses it.
>
> Two things. First, **Santa Barbara absorbs the squid** — plus 5.8 points for the squid specialist, plus 3.4 for the roving generalist, plus 2.1 for the forage-diverse fleet. And the fleets go out **more often**: non-participation falls by 6 and 4 points for the two southern squid fleets. The Pacific Northwest sardine fleet barely moves — it has no squid to follow.
>
> Second, and this is the interesting one — [apuntar al panel **abajo a la derecha**, *S. CCS forage diverse*] — **Los Angeles goes down**, minus 0.6. But its *squid* landings go **up**. It loses effort because it loses **chub mackerel**, almost two full points. The port-level total **hides a recomposition of the species mix.**"

**Frase para cerrar (es la que habla directamente a la sesión):**

> "And note what this machinery is: we moved a habitat field and got a spatial reallocation. **A closed area is the same operation** — you mask the field inside the polygon and re-predict. That's the door the SDM opens."

**Puntero:** primero el panel superior izquierdo (squid specialist, la barra grande de Santa Barbara), después el **inferior derecho** (forage diverse) para la historia de LA. **Ojo: la historia de Los Angeles es la del panel forage-diverse** (LA = −0.59, chub mackerel = −1.88), no la del squid specialist.

**Cuidado:** el escenario es una **redistribución dentro de la misma temporada**, no una proyección climática. Si se dice "climate scenario", alguien va a preguntar por el forzante y no lo hay.

**Transición:** "Why did LA lose effort while its squid went up? One slide."

---

## Slide 9 — Estructura de sustitución `[CUT]`

**Este es el slide que se bota primero si la sesión va atrasada.** *Explica* el anterior, no agrega un resultado nuevo, y el slide de takeaways igual cierra el argumento. Si se bota, decir en su lugar una sola frase al pasar al slide 10: *"and the reason LA behaves that way is that for that fleet, the closest substitute is another species, not another port."*

**Idea única:** dos formas de adaptarse — *on the move* (seguir la especie entre puertos) y *in place* (cambiar de especie en el mismo puerto).

**Guion:**

> "The nesting parameters tell you *how* each fleet adapts. Lower lambda means alternatives inside the nest are **closer substitutes**.
>
> For most segments the nests are **species**, and the lambdas are low — anchovy 0.49, sardine 0.40. That means: given the species, the **ports** are close substitutes. Those fleets adapt **on the move** — they follow the species along the coast.
>
> The forage-diverse segment nests by **port**: Santa Barbara 0.49 against 0.73 for Los Angeles and Monterey. There, substitution happens **across species within a port**. That fleet adapts **in place** — and that's why Los Angeles lost effort even as its squid went up: its closest substitute was another species, and that species left.
>
> The terminology — 'on the move' versus 'in place' — is from Samhouri and co-authors, 2024."

**Transición:** "Three things to take away."

---

## Slide 10 — Takeaways

**Idea única:** SDMs sirven como regresores económicos; los pescadores son flexibles; y la respuesta espacial pasa por el portafolio.

**Guion:**

> "Three points.
>
> **One:** oceanographic SDMs work as **economic regressors** — exogenous, gap-free, daily. And the estimated models predict a **held-out year** well: out-of-sample pseudo-R-squared between **0.22 and 0.44** against the null model.
>
> **Two:** harvesters are **flexible**. Participation, target and landing all respond to availability, prices, costs and weather — and they do so *even after* controlling for very strong state dependence.
>
> **Three:** the spatial answer runs **through the portfolio**. Even the fleet that adapts *in place* — substituting **species** rather than ports — ends up **redistributing effort in space**. If you model one species at a time, you miss that channel entirely.
>
> And because availability here is an **ocean-model output**, the same machinery runs forward under **climate projections**, or under a closed area. Thank you."

**⚠️ Cuidado crítico:** el 0.22–0.44 es **contra el modelo nulo**, *no* contra una especificación con catch pasado. **Nunca** decir "the SDM predicts better than past catch" — no corrimos esa comparación. El argumento del SDM es de **identificación y de escenarios**, no de superioridad predictiva. (Ver Q&A #1.)

---

## Slide 11 — Thanks

Dejarlo en pantalla durante las preguntas. No leerlo. Sólo: *"Thank you — happy to take questions."*

---

## Plan comprimido (6–7 min)

Hablamos últimos: es probable que haya que hacer esto. Orden de sacrificio:

1. **Botar el slide 9** (substitution) — pasar directo de 8 a 10 con la frase puente de una línea.
2. **Comprimir el slide 6** (modelo) a 25 s: *"Nested logit over participate × target × port. Availability enters daily and is exogenous; we also control for prices, distances, wind and a thirty-day state-dependence term. Estimated in Apollo."* y pasar.
3. **Comprimir el slide 5** (setting) a 20 s: *"Four fleet segments from a cluster analysis, four different species portfolios."*
4. Si aún así falta tiempo: **botar el 4** y meter su idea en una frase dentro del 3 (*"the usual proxy is past catch, which is endogenous, gappy, and — crucially — impossible to shift in a counterfactual"*).

**Nunca** botar: 3 (contribución) y 8 (payoff). Con esos dos y el 10 la charla se sostiene en 4 minutos.

---

## Números para tener en la cabeza

| Dato | Valor |
|---|---|
| Período | 2013–2017 (cierre de sardina 2015; ola de calor 2014–16) |
| Trips totales | ~41,000 (29,160 / 6,806 / 2,581 / 2,481) |
| Revenue de la especie dominante | 93.6% |
| State dependence (30 d) | +2.3 a +3.1, *** en los cuatro segmentos |
| Radios del SDM | 60 km (sardina/anchoveta/caballa), 90 km (calamar) |
| Escenario: centroide del calamar | 42.99°N → 40.66°N (mismo período estacional) |
| Δ Santa Barbara | +5.8 / +3.4 / — / +2.1 pp |
| Δ Los Angeles (forage diverse) | −0.6 neto (calamar +1.5, chub mackerel −1.9) |
| Δ participación | +6.0 / +4.2 / −0.6 / +1.9 pp |
| Δ target calamar | +8.4 (squid spec.) / +4.5 (roving) / +5.0 (forage div.) pp |
| λ más bajos | anchoveta 0.49, sardina 0.40, Santa Barbara 0.49 |
| OOS pseudo-ρ² (año retenido, **vs. nulo**) | 0.22–0.44 |
| Correlación anchoveta–calamar (disponibilidad) | r = −0.29 |

---

## Banco de preguntas

Formato de respuesta: **una frase de respuesta directa, una de sustento, parar.** No dar mini-charlas.

**1. "¿Compararon el SDM contra el catch pasado? ¿Predice mejor?"** — *La pregunta más probable, y la más fácil de contestar mal.*

> "No — we didn't run that horse race, and I want to be precise about it. Our case for the SDM is **identification and counterfactuals**, not predictive superiority: past catch is endogenous to the choice we're modeling, and you can't shift it in a scenario. What we *do* show is that the estimated models predict a held-out year well against the null, and Table S1 compares SDM **timing** variants — daily, one-day lag, seven-, fourteen- and thirty-day moving averages. A direct SDM-versus-past-catch comparison is a good idea for future work."

**2. "Habitat suitability no es lo mismo que tasa de captura."**

> "Agreed, and that's our main caveat. Suitability is a proxy for potential catch rates. The good news is the direction of the bias: classical measurement error in a regressor **attenuates** the coefficient. So our availability effects are, if anything, a lower bound."

**3. "¿Los precios no son endógenos?"**

> "Partly, yes. We use **predicted** prices from a year-trend plus month and port fixed effects, and check robustness with thirty-day moving averages. That reduces simultaneity but doesn't eliminate it, so we read price coefficients as **reduced-form** behavioural responses, not structural supply elasticities."

**4. "¿Por qué la anchoveta sale negativa para el forage-diverse?"**

> "Because it's an opportunity cost, not a dislike of anchovy. In that region anchovy and squid availability are **negatively correlated** — r of about −0.29 — so high anchovy days are low squid days for a fleet whose best option is squid."

**5. "¿Podrían correr un cierre por eólica offshore / un AMP?"**

> "Yes — it's the same operation as the scenario I showed. You mask the habitat field inside the closure polygon, recompute the port-radius averages, and re-predict. We haven't run it in this paper, but nothing in the machinery has to change. That's exactly why the exogenous field matters."

**6. "¿Por qué las estructuras de nidos difieren entre segmentos?"**

> "It's empirical. We tried both structures for each segment and kept the one the data supported — fit, and dissimilarity parameters inside the unit interval. For the forage-diverse fleet the port nesting wins, and that's substantively informative: it *is* the finding about adapting in place."

**7. "¿Agregación a nivel de puerto no oculta heterogeneidad espacial fina?"**

> "It does — we list it as our first limitation, following Dépalle and co-authors, 2021. Our choice set is port × species, so within-port location choice is averaged out. The trade-off buys us the joint participation–target–location model; a finer grid would make the choice set intractable."

**8. "¿Cómo separan state dependence de heterogeneidad no observada?"**

> "Honestly, not perfectly — that's the classic initial-conditions problem. What I'd emphasise is the direction: the thirty-day term absorbs a lot of persistence, so it makes it **harder** for availability to show up. The availability effects survive that, which is the point of the slide."

**9. "¿Por qué sólo 2013–2017?"**

> "Two reasons: it's the window where the daily SDM fields and the trip-level PacFIN data overlap cleanly, and it contains the two shocks we care about — the 2015 sardine closure and the 2014–16 marine heatwave."

**10. "¿Modelan entrada y salida de embarcaciones?"**

> "No. Our extensive margin is the **daily** decision to go fishing or not, conditional on being in the fleet. Long-run exit and vessel investment are a different model."

**11. "¿Propagan la incertidumbre del SDM?"**

> "Not in this paper — availability enters as a fixed regressor. That's measurement error, so again it attenuates. Propagating the SDM's predictive distribution through the choice model is a clear next step."

**12. "Los coeficientes de no-CPS son enormes (+10.3 crab en PNW)."**

> "They are, and they're doing a specific job: crab and salmon availability essentially **gate participation** for the fleets that have those permits. They sit in a separate nest because switching gear is costly, so those magnitudes aren't comparable to the CPS terms."

**13. "¿Y bajo proyecciones climáticas?"**

> "The SDMs are driven by ROMS output, so in principle you force them with downscaled projections and run the same behavioural model forward. That's the direction we're taking it — with the caveat that the behavioural parameters are assumed stable, which is a strong assumption over decades."

---

## Backups: mapa rápido

Saber en qué orden están, para llegar de una:

1. **Descriptives** — persistencia + switching (squid specialist: 19.8 cambios de especie, run mediano 2 días). *Úsalo si preguntan por qué hay state dependence.*
2. **Elasticities** — precio mueve la **participación**, disponibilidad mueve el **targeting**. *Úsalo si preguntan por magnitudes (el slide 7 no las da).*
3. **Scenario — participación** (gráfico de barras). *Si preguntan cuánto más salen a pescar.*
4. **Scenario — por especie objetivo**. *Si preguntan qué especie ganó/perdió.*
5. **Scenario — por puerto × especie**. *La mejor para la historia de LA.*
6. **λ (dissimilarity)** — tabla completa. *Si cuestionan la estructura de nidos.*
7. **Variable construction** — SDM, precios, costos, viento. *Si preguntan por los radios o ERA5.*
8. **Limitations** — las cuatro, escritas. *Si la pregunta es incómoda, ir directo aquí: se ve mejor tener la limitación en un slide que improvisarla.*

---

## Recordatorios de entrega

- **Hablamos últimos.** Preguntar al chair al llegar cuánto tiempo real hay. Si dice "eight minutes", aplicar el plan comprimido **desde el principio**, no improvisar recortes a mitad de charla.
- Mirar a la audiencia en las cuatro frases marcadas (slides 3, 4, 7, 8). El resto se puede leer del apuntador.
- El mapa del slide 3 es **ilustrativo** (Chasco et al. 2022) — decirlo antes de que lo pregunten.
- Nunca afirmar que el SDM **predice mejor** que el catch pasado.
- Si se acaba el tiempo en el slide 8: saltar directo al 10 y decir sólo el punto 3 + la frase de cierre.
