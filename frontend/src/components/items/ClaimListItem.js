import React, { Component, PropTypes } from 'react'

import ActionThumbDown from 'material-ui/svg-icons/action/thumb-down';
import ActionThumbUp from 'material-ui/svg-icons/action/thumb-up';
import Badge from 'material-ui/Badge';
import IconButton from 'material-ui/IconButton';

import AuthoredListItem from './AuthoredListItem'

const styles = {
  leftElem: {
    display: 'flex',
    flexDirection: 'row'
  },
  thumbContainer: {
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center'
  },
  thumb: {
    paddingBottom: 0,
  },
  forBadge: {
    color: 'green',
    fontSize: 10
  },
  againstBadge: {
    color: 'red',
    fontSize: 10
  }
}

const ClaimListItem = (props) => {
  const { claim, router, linksOff } = props
  const { argsFor, argsAgainst } = claim

  const goToClaim = () => (!linksOff && router.push(`/claims/${claim.id}`))
  const goToAuthor = () => (!linksOff && router.push(`/users/${claim.authorId}`))

  const leftElem = (
    <div style={styles.leftElem}>

      <div style={styles.thumbContainer}>
        <IconButton
          tooltip={`${argsFor.length} arguments for`}
          tooltipPosition="top-left"
          style={styles.thumb}
        >
          <ActionThumbUp />
        </IconButton>
        <span style={styles.forBadge}>
          {String(argsFor.length)}
        </span>
      </div>

      <div style={styles.thumbContainer}>
        <IconButton
          tooltip={`${argsAgainst.length} arguments against`}
          tooltipPosition="top-right"
          style={styles.thumb}
        >
          <ActionThumbDown />
        </IconButton>
        <span style={styles.againstBadge}>
          {String(argsAgainst.length)}
        </span>
      </div>

    </div>
  )

  return (
    <AuthoredListItem
      text={claim.text}
      authorId={claim.authorId}
      authorName={claim.authorName}
      onTouchTap={e => goToClaim()}
      onAuthorTouchTap={e => goToClaim()}
      leftElem={leftElem}
    />
  )
}

export default ClaimListItem
