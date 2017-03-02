import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import {List} from 'material-ui/List';
import Toggle from 'material-ui/Toggle';

import AuthoredListItem from './AuthoredListItem';

const styles = {
  block: {
    maxWidth: 250,
  },
  toggle: {
    marginBottom: 16,
  },
  thumbOff: {
    backgroundColor: '#ffcccc',
  },
  trackOff: {
    backgroundColor: '#ff9d9d',
  },
  thumbSwitched: {
    backgroundColor: 'red',
  },
  trackSwitched: {
    backgroundColor: '#ff9d9d',
  },
  labelStyle: {
    color: 'red',
  },
}


class Claim extends Component {

  constructor(props) {
    super(props)
    this.state = {
      items: [],
      forAgainst: 'For'
    }
  }

  componentDidMount() {
    //TODO make request for arguments and set state
  }

  render() {
    const { claim } = this.props
    const { items, forAgainst } = this.state

    const shownItems = items.filter(item => item.forAgainst == forAgainst)

    return (
      <Card>
        <CardTitle title={claim.text} subtitle={claim.author} />
        <Toggle
          label={forAgainst}
          thumbStyle={styles.thumbOff}
          trackStyle={styles.trackOff}
          thumbSwitchedStyle={styles.thumbSwitched}
          trackSwitchedStyle={styles.trackSwitched}
          labelStyle={styles.labelStyle}
          onToggle={(e, i) => this.toggleForAgainst()}
        />
        <CardText>
          <List>
            { shownItems.map(item =>
                <AuthoredListItem
                  text={item.argumentText}
                  authorId={item.argumentAuthorId}
                  authorName={item.argumentAuthorName}
                />
              )
            }
          </List>
        </CardText>
        <CardActions>
          <FlatButton label="Action1" />
          <FlatButton label="Action2" />
        </CardActions>
      </Card>
    )
  }

  toggleForAgainst() {
    const { forAgainst } = this.state
    this.setState({
      forAgainst: forAgainst == 'For' ? 'Against' : 'For'
    })
  }
}
