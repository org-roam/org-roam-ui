export const checkFileSystemCompatibility = () => {
  return typeof window.showDirectoryPicker !== 'undefined'
}
