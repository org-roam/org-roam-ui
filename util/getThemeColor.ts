export const getThemeColor = (name: string, theme: any) => {
  return name.split('.').reduce((o, i) => o[i], theme.colors)
}
