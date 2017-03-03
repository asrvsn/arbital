import React, { Component, PropTypes } from 'react'

const goToItem = (props) => {
  props.history.push(props.hrefPath)
}

const AuthoredListItem = (props) => {
  const { text, authorName, authorId, hrefPath } = props
  const authorIcon = <AuthorChip authorName={authorName} authorId={authorId} />

  return (
    <ListItem
      primaryText={text}
      rightIcon={authorIcon}
      onTouchTap={e => goToItem(props)}
    />
  )
}

export default AuthoredListItem
