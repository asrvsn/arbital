import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import ActionThumbDown from 'material-ui/svg-icons/action/thumb-down';
import ActionThumbUp from 'material-ui/svg-icons/action/thumb-up';
import Badge from 'material-ui/Badge';
import IconButton from 'material-ui/IconButton';

import AuthoredListItem from './AuthoredListItem'
import { openDialog } from '../../actions'

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
  const { claim, router, linksOff, dispatch } = props
  const { argsFor, argsAgainst } = claim

  let goToClaim, goToAuthor, openArgumentForDialog, openArgumentAgainstDialog;
  if (linksOff) {
    goToClaim = () => {}
    goToAuthor = () => {}
    openArgumentForDialog = () => {}
    openArgumentAgainstDialog = () => {}
  } else {
    goToClaim = () => router.push(`/claims/${claim.id}`)
    goToAuthor = () => router.push(`/users/${claim.authorId}`)
    openArgumentForDialog = (e) => {
      e.preventDefault()
      e.stopPropagation()
      dispatch(openDialog({
        dialogType: 'CREATE_ARGUMENT',
        payload: { linkedClaimId: claim.id, isFor: true }
      }))
    }
    openArgumentAgainstDialog = (e) => {
      e.preventDefault()
      e.stopPropagation()
      dispatch(openDialog({
        dialogType: 'CREATE_ARGUMENT',
        payload: { linkedClaimId: claim.id, isFor: false }
      }))
    }
  }

  const leftElem = (
    <div style={styles.leftElem}>

      <div style={styles.thumbContainer}>
        <IconButton
          tooltip={`${argsFor.length} arguments for`}
          tooltipPosition="top-right"
          style={styles.thumb}
          onTouchTap={e => openArgumentForDialog(e)}
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
          onTouchTap={e => openArgumentAgainstDialog(e)}
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
      subtext={`Created ${claim.creationDate}`}
      authorId={claim.authorId}
      authorName={claim.authorName}
      onTouchTap={e => goToClaim()}
      onAuthorTouchTap={e => goToClaim()}
      leftElem={leftElem}
    />
  )
}

export default connect(
  null,
  (dispatch) => ({ dispatch })
)(ClaimListItem)
