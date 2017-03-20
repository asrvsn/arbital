import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';
import Toggle from 'material-ui/Toggle';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  block: {
  },
  toggle: {
    marginLeft: 16,
  },
  thumbOff: {
    backgroundColor: '#388E3C',
  },
  trackOff: {
    backgroundColor: '#81C784',
  },
  thumbSwitched: {
    backgroundColor: '#D32F2F',
  },
  trackSwitched: {
    backgroundColor: '#E57373',
  },
  labelStyle: {
  },
}

class Claim extends Component {

  constructor(props) {
    super(props)
    this.state = {
      showingFor: true
    }
  }

  render() {
    const { page, router } = this.props
    const { claim, argsFor, argsAgainst } = page
    const { showingFor } = this.state

    const shownItems = showingFor ? argsFor : argsAgainst

    return (
      <Card>
        <CardTitle title={claim.text} subtitle={claim.authorName} />
        <Toggle
          style={styles.toggle}
          label={'Showing arguments ' + (showingFor ? 'for' : 'against')}
          thumbStyle={styles.thumbOff}
          trackStyle={styles.trackOff}
          thumbSwitchedStyle={styles.thumbSwitched}
          trackSwitchedStyle={styles.trackSwitched}
          labelStyle={styles.labelStyle}
          onToggle={(e, i) => this.toggleForAgainst()}
          labelPosition="right"
        />
        <CardText>
          <List>
            { (shownItems.length > 0) ?
              shownItems.map(item =>
                <AuthoredListItem
                  text={item.text}
                  authorId={item.authorId}
                  authorName={item.authorName}
                  onTouchTap={router.push('/arguments/' + item.id)}
                />
              )
            :
              <ListItem primaryText="Nothing here" />
            }
          </List>
        </CardText>
      </Card>
    )
  }

  toggleForAgainst() {
    this.setState({
      showingFor: ! this.state.showingFor
    })
  }
}

export default GetterHOC(
  Claim,
  (props) => ({
    claim: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    }
  })
)
