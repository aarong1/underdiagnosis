tags$head(tags$style('/* Ion.RangeSlider x Bootswatch */
  .irs {
    font-family: var(--bs-body-font-family, inherit);
  }
.irs .irs-line, .irs .irs-bar {
  height: .4rem;
  border-radius: var(--bs-border-radius, .375rem);
}
.irs .irs-bar {
  background-color: var(--bs-primary);
}
.irs .irs-handle {
  width: 1rem; height: 1rem;
  top: 22px; /* fine-tune vertically per theme */
    border-radius: 50%;
  border: 2px solid var(--bs-primary);
  background: var(--bs-body-bg);
  box-shadow: 0 0 0 .2rem color-mix(in srgb, var(--bs-primary) 25%, transparent);
}
.irs--flat .irs-grid-text {
  color: var(--bs-secondary-color);
}
'))