import React, { Component, PropTypes } from 'react'

import AuthoredListItem from './AuthoredListItem'

const ArgumentListItem = (props) => {
  const { argument, router } = props

  const goToArg = () => router.push(`/arguments/${argument.id}`)
  const goToAuthor = () => router.push(`/users/${argument.authorId}`)

  return (
    <AuthoredListItem
      text={argument.text}
      subtext={`Created ${argument.creationDate}`}
      authorId={argument.authorId}
      authorName={argument.authorName}
      onTouchTap={e => goToArg()}
      onAuthorTouchTap={e => goToAuthor()}
    />
  )
}

export default ArgumentListItem
