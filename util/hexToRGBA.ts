export function hexToRGBA(hex: string, opacity: number) {
  return (
    'rgba(' +
    (hex = hex.replace('#', ''))
      .match(new RegExp('(.{' + hex.length / 3 + '})', 'g'))!
      .map((l) => parseInt(hex.length % 2 ? l + l : l, 16))
      .concat(isFinite(opacity) ? opacity : 1)
      .join(',') +
    ')'
  )
}
