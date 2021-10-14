import React, { useEffect, useState } from 'react'
import Image from 'next/image'
import path from 'path'
//import '../../../public/placeholder.png'

export interface OrgImageProps {
  src: string
  file: string
}

export const OrgImage = (props: OrgImageProps) => {
  const { src, file } = props

  const [image, setImage] = useState<any>(null)

  /* )
*   .then((res) => res.blob())
*   .then((res) => setImage(res))
*   .catch((e) => {
*     setImage(null)
*     console.error(e)
*   })
}, [fullPath]) */

  const dumbLoader = ({ src, width, quality }: { [key: string]: string | number }) => {
    return `${src}`
  }
  const homeLoader = ({ src, width, quality }: { [key: string]: string | number }) => {
    return `http://localhost:35901/img/${src}`
  }

  if (src.replaceAll(/(http)?.*/g, '$1')) {
    console.log(src.replaceAll(/(http)?.*/g, '$1'))
    return (
      <Image layout="responsive" loader={dumbLoader} src={src} alt="" width="100%" height="100%" />
    )
  }

  const srcName = src.replaceAll(/file:/g, '')

  const dir = path.dirname(file)
  const fullPath =
    path.isAbsolute(srcName) || srcName.slice(0, 1) === '~' ? srcName : path.join(dir, srcName)
  const encodedPath = encodeURIComponent(encodeURIComponent(fullPath))

  return (
    <Image
      layout="responsive"
      loader={homeLoader}
      src={encodedPath}
      alt=""
      width="100%"
      height="100%"
    />
  )
}
